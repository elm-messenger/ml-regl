#!/usr/bin/env python3
"""Check that both hosts draw test_texture_parity identically and correctly.

Every slot of test/test_texture_parity.ml shows cells of
test/assets/orientation.png (4x4 cells, 16 colours). This script captures the
desktop window (over the control protocol) and the browser page (headless
Chrome), samples the centre of every drawn cell, and checks it against the
cell the slot must show. It also compares the label text of both hosts, so a
flipped font atlas fails too, checks that a compositor with one empty side
treats it as a transparent image, and checks the text rows: each is drawn
left-aligned and right-aligned at its Regl_text width, so both copies' ink
must line up, on both hosts alike.

Build first: ./build.sh (desktop executable and test bundles) and the
ml-regl-js bundle (`make build` in ml-regl-js). Needs Python's `websockets`
and Pillow, and google-chrome. The desktop window is open for a few seconds.

usage: python3 test/check_texture_parity.py [--keep DIR]
"""

from __future__ import annotations

import argparse
import asyncio
import functools
import http.server
import json
import os
from pathlib import Path
import subprocess
import tempfile
import threading

from PIL import Image
import websockets

ROOT = Path(__file__).resolve().parents[1]
VIRTUAL = (1280, 720)
BACKGROUND = (128, 128, 128)
COLORS = [
    (255, 0, 0), (0, 255, 0), (0, 0, 255), (255, 255, 0),
    (255, 0, 255), (0, 255, 255), (128, 0, 0), (0, 128, 0),
    (0, 0, 128), (128, 128, 0), (128, 0, 128), (0, 128, 128),
    (255, 128, 0), (0, 0, 0), (255, 255, 255), (128, 255, 128),
]
WHOLE = (0, 0, 4, 4, False)
TOP_RIGHT = (2, 0, 2, 2, False)
# (label, (first column, first row, columns, rows, rows reversed)) in slot
# order; must match [probes] in test_texture_parity.ml.
PROBES = [
    ("rect_texture", WHOLE),
    ("rect_texture alpha", WHOLE),
    ("centered_texture", WHOLE),
    ("centered alpha", WHOLE),
    ("texture", WHOLE),
    ("texture alpha", WHOLE),
    ("rect_texture_cropped", TOP_RIGHT),
    ("rect cropped alpha", TOP_RIGHT),
    ("centered cropped", TOP_RIGHT),
    ("centered cropped alpha", TOP_RIGHT),
    ("texture_cropped", TOP_RIGHT),
    ("texture_cropped alpha", TOP_RIGHT),
    ("crop at load", TOP_RIGHT),
    ("flip_y", (0, 0, 4, 4, True)),
    ("crop at load + flip_y", (2, 0, 2, 2, True)),
    # The flipped texture's top-right 2x2 cells are the image's rows 3 and 2.
    ("flip_y + draw crop", (2, 2, 2, 2, True)),
    ("custom program", WHOLE),
    ("custom effect", WHOLE),
    ("built-in effect", WHOLE),
    ("custom compositor", WHOLE),
    ("built-in compositor", WHOLE),
    ("compositor, empty 2nd", WHOLE),
    ("compositor, empty 1st", WHOLE),  # shows only the background
    ("fade, empty side", WHOLE),  # each cell half-way to the background
]
BACKGROUND_ONLY = {"compositor, empty 1st"}
HALF_FADED = {"fade, empty side"}
TOLERANCE = 40
# Text rows (see [text_rows] in test_texture_parity.ml): the y of the
# left-aligned copy; the right-aligned copy is 30 below.
TEXT_ROWS = [("letter spacing", 560), ("tabs and word spacing", 630)]


def samples():
    """Yield (probe label, virtual x, y, expected colour)."""
    for index, (label, (c0, r0, cols, rows, reversed_rows)) in enumerate(PROBES):
        sx, sy = index % 8 * 160 + 16, index // 8 * 180 + 8
        for j in range(rows):
            for i in range(cols):
                row = r0 + (rows - 1 - j if reversed_rows else j)
                x = sx + (i + 0.5) * 128 / cols
                y = sy + (j + 0.5) * 128 / rows
                color = COLORS[row * 4 + c0 + i]
                if label in BACKGROUND_ONLY:
                    color = BACKGROUND
                elif label in HALF_FADED:
                    color = tuple((a + b) // 2 for a, b in zip(color, BACKGROUND))
                yield label, x, y, color


class Canvas:
    """A screenshot plus where the 1280x720 virtual area lies in it."""

    def __init__(self, path: Path):
        self.image = Image.open(path).convert("RGB")
        px = self.image.load()
        w, h = self.image.size
        near = lambda c: all(abs(a - b) < 12 for a, b in zip(c, BACKGROUND))
        # The clear colour fills the canvas edges, so its bounding box is the
        # virtual area (the desktop letterbox and the page around are not it).
        xs = [x for x in range(0, w, 2) for y in (h // 2, h * 9 // 10) if near(px[x, y])]
        ys = [y for y in range(0, h, 2) for x in (w // 2, w * 99 // 100) if near(px[x, y])]
        if not xs or not ys:
            raise SystemExit(f"{path}: no canvas found (blank capture?)")
        self.left, self.right = min(xs), max(xs) + 1
        self.top, self.bottom = min(ys), max(ys) + 1

    def at(self, x: float, y: float):
        px = self.left + x * (self.right - self.left) / VIRTUAL[0]
        py = self.top + y * (self.bottom - self.top) / VIRTUAL[1]
        return self.image.getpixel((int(px), int(py)))

    def ink_columns(self, y0: float, y1: float):
        """Leftmost and rightmost virtual x with text ink in [y0, y1)."""
        xs = []
        for vy in range(int(y0), int(y1)):
            for vx in range(VIRTUAL[0]):
                if sum(self.at(vx, vy)) > 600:
                    xs.append(vx)
        return (min(xs), max(xs)) if xs else None

    def labels(self, index: int) -> Image.Image:
        """The slot's label strip, scaled to virtual size, in grey."""
        sx, sy = index % 8 * 160 + 16, index // 8 * 180 + 140
        box = [self.left + sx * (self.right - self.left) / VIRTUAL[0],
               self.top + sy * (self.bottom - self.top) / VIRTUAL[1],
               self.left + (sx + 140) * (self.right - self.left) / VIRTUAL[0],
               self.top + (sy + 16) * (self.bottom - self.top) / VIRTUAL[1]]
        return self.image.crop(tuple(int(v) for v in box)).convert("L").resize((140, 16))


async def capture_desktop(out: Path) -> None:
    messages: asyncio.Queue[dict] = asyncio.Queue()
    socket_ready = asyncio.get_running_loop().create_future()

    async def handler(socket):
        if not socket_ready.done():
            socket_ready.set_result(socket)
        try:
            async for raw in socket:
                messages.put_nowait(json.loads(raw))
        except websockets.exceptions.ConnectionClosed:
            pass

    async with websockets.serve(handler, "127.0.0.1", 0) as server:
        port = server.sockets[0].getsockname()[1]
        env = dict(os.environ, DECLGL_DEBUG="1", DECLGL_CONTROL_URL=f"ws://127.0.0.1:{port}")
        process = subprocess.Popen(
            [str(ROOT / "_build/default/test/test_texture_parity_desktop.exe")],
            cwd=ROOT / "test", env=env,
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        )
        try:
            socket = await asyncio.wait_for(socket_ready, 10.0)

            async def command(method, params, command_id):
                await socket.send(json.dumps({"method": method, "id": command_id, "params": params}))
                while True:
                    item = await asyncio.wait_for(messages.get(), 10.0)
                    if item.get("type") == "response" and item.get("id") == command_id:
                        assert item["ok"], item
                        return item["result"]

            await asyncio.sleep(2.0)  # assets and programs load
            await command("screenshot", {"path": str(out)}, 1)
            await command("quit", {}, 2)
            process.wait(timeout=10)
        finally:
            if process.poll() is None:
                process.terminate()
                process.wait(timeout=10)


class QuietHandler(http.server.SimpleHTTPRequestHandler):
    def log_message(self, *args):
        pass


def capture_browser(out: Path) -> None:
    handler = functools.partial(QuietHandler, directory=str(ROOT))
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    try:
        with tempfile.TemporaryDirectory() as profile:
            subprocess.run(
                ["google-chrome", "--headless=new", "--use-angle=swiftshader",
                 "--enable-unsafe-swiftshader", f"--user-data-dir={profile}",
                 "--hide-scrollbars", "--window-size=1600,1000",
                 "--virtual-time-budget=8000", f"--screenshot={out}",
                 f"http://127.0.0.1:{server.server_address[1]}/html/test_texture_parity.html"],
                check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=120,
            )
    finally:
        server.shutdown()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--keep", type=Path, help="directory to keep the two captures in")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory() as tmp:
        out = args.keep or Path(tmp)
        out.mkdir(parents=True, exist_ok=True)
        desktop_path, browser_path = out / "desktop.bmp", out / "browser.png"
        asyncio.run(capture_desktop(desktop_path))
        capture_browser(browser_path)
        hosts = {"desktop": Canvas(desktop_path), "browser": Canvas(browser_path)}

        failures = []
        for label, x, y, expected in samples():
            for host, canvas in hosts.items():
                got = canvas.at(x, y)
                if max(abs(a - b) for a, b in zip(got, expected)) > TOLERANCE:
                    failures.append(f"{host}: {label} at ({x:.0f}, {y:.0f}): expected {expected}, got {got}")
        extents = {}
        for name, y in TEXT_ROWS:
            for host, canvas in hosts.items():
                left = canvas.ink_columns(y, y + 28)
                right = canvas.ink_columns(y + 30, y + 58)
                if not left or not right:
                    failures.append(f"{host}: text row '{name}' has no ink")
                    continue
                if max(abs(a - b) for a, b in zip(left, right)) > 1:
                    failures.append(f"{host}: text row '{name}': left-aligned ink {left}, right-aligned at the measured width {right}")
                extents.setdefault(name, {})[host] = left
            got = extents.get(name, {})
            if len(got) == 2 and max(abs(a - b) for a, b in zip(got["desktop"], got["browser"])) > 1:
                failures.append(f"text row '{name}' differs between hosts: {got}")
        for index, (label, _) in enumerate(PROBES):
            a, b = hosts["desktop"].labels(index), hosts["browser"].labels(index)
            diff = sum(abs(p - q) for p, q in zip(a.tobytes(), b.tobytes())) / (140 * 16)
            if diff > 30:
                failures.append(f"label '{label}' differs between hosts (mean difference {diff:.0f})")

    for failure in failures:
        print(failure)
    cells = sum(1 for _ in samples())
    print(f"{len(PROBES)} slots, {cells} cells per host: "
          + ("OK" if not failures else f"{len(failures)} failures"))
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
