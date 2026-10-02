#!/usr/bin/env python3
"""Exercise the native ml-regl control client over a real WebSocket.

Drives test_control_frames, whose clear colour changes every frame: after
each single step, the screenshot must show the frame the render tree holds.
Also checks that a virtual-scale view capture has exactly the virtual size,
that a relative path is relative to the game's working directory (missing
directories are created), and that a failed write names the path.

Build first (./build.sh); the window is open for a few seconds.
"""

from __future__ import annotations

import asyncio
import json
import os
from pathlib import Path
import struct
import subprocess
import tempfile

import websockets

COLORS = {(255, 0, 0), (0, 255, 0), (0, 0, 255), (255, 255, 0)}


def bmp_info(path: Path):
    """(width, height, colour of the centre pixel) of a 24/32-bit BMP."""
    data = path.read_bytes()
    offset, = struct.unpack_from("<I", data, 10)
    width, height = struct.unpack_from("<ii", data, 18)
    bpp, = struct.unpack_from("<H", data, 28)
    stride = (width * bpp // 8 + 3) & ~3
    row = abs(height) // 2
    if height > 0:  # bottom-up
        row = height - 1 - row
    i = offset + row * stride + width // 2 * (bpp // 8)
    b, g, r = data[i], data[i + 1], data[i + 2]
    if bpp == 32:  # SDL writes RGBA32 surfaces with explicit masks
        masks = struct.unpack_from("<IIII", data, 54)
        pixel, = struct.unpack_from("<I", data, i)
        r, g, b = ((pixel & m) >> ((m & -m).bit_length() - 1) for m in masks[:3])
    return width, abs(height), (r, g, b)


def tree_color(tree) -> tuple[int, int, int]:
    fields = tree["atomic"]["fields"]
    color = next(f["value"]["numbers"] for f in fields if f["key"] == "color")
    return tuple(round(c * 255) for c in color[:3])


async def run() -> None:
    messages: asyncio.Queue[dict] = asyncio.Queue()
    socket_ready = asyncio.get_running_loop().create_future()

    latest_frame = [-1]

    async def handler(socket):
        if not socket_ready.done():
            socket_ready.set_result(socket)
        try:
            async for raw in socket:
                message = json.loads(raw)
                if message.get("type") == "frame":
                    latest_frame[0] = message["frame"]
                messages.put_nowait(message)
        except websockets.exceptions.ConnectionClosed:
            pass

    async with websockets.serve(handler, "127.0.0.1", 0) as server:
        port = server.sockets[0].getsockname()[1]
        root = Path(__file__).resolve().parents[1]
        binary = root / "_build/default/test/test_control_frames_desktop.exe"
        workdir = Path(tempfile.mkdtemp(prefix="ml-regl-native-control-"))
        env = os.environ.copy()
        env["DECLGL_DEBUG"] = "1"
        env["DECLGL_CONTROL_URL"] = f"ws://127.0.0.1:{port}"
        process = subprocess.Popen(
            [str(binary)],
            cwd=workdir,
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
        )

        async def next_message(predicate, timeout=5.0):
            while True:
                message = await asyncio.wait_for(messages.get(), timeout)
                if predicate(message):
                    return message

        try:
            socket = await asyncio.wait_for(socket_ready, 5.0)
            hello = await next_message(lambda message: message.get("type") == "hello")
            assert hello["protocol"] == 1
            assert "step" in hello["capabilities"]

            ids = iter(range(1, 1000))

            async def command(method, params=None, ok=True):
                command_id = next(ids)
                message = {"method": method, "id": command_id}
                if params is not None:
                    message["params"] = params
                await socket.send(json.dumps(message))
                response = await next_message(
                    lambda item: item.get("type") == "response"
                    and item.get("id") == command_id
                )
                assert response["ok"] == ok, response
                return response["result" if ok else "error"]

            await asyncio.sleep(1.0)  # let the window settle
            assert (await command("pause"))["paused"]
            state = await command("get_state")
            assert state["paused"] is True
            assert (await command("input", {"kind": "mouse_move", "x": 20, "y": 30}))["delivered"]

            # Every single step: the screenshot holds the frame just drawn.
            for n in range(4):
                frame = (await command("get_state"))["frame"]
                assert (await command("step", {"frames": 1, "dt_ms": 10}))["queued"] == 1
                for _ in range(500):
                    if latest_frame[0] >= frame + 1:
                        break
                    await asyncio.sleep(0.01)
                assert latest_frame[0] >= frame + 1, (latest_frame, frame)
                tree = await command("get_render_tree")
                assert tree["available"] is True
                capture = await command("screenshot", {
                    "path": f"shots/{n}.bmp", "area": "view", "scale": "virtual"})
                path = Path(capture["path"])
                assert path == workdir / "shots" / f"{n}.bmp", capture
                width, height, color = bmp_info(path)
                assert (width, height) == (1280, 800), (width, height, capture)
                assert (capture["width"], capture["height"]) == (1280, 800), capture
                assert color in COLORS and color == tree_color(tree["tree"]), \
                    f"step {n}: screenshot {color}, render tree {tree_color(tree['tree'])}"

            error = await command("screenshot", {"path": "/proc/no/such/dir/x.png",
                                                 "format": "png"}, ok=False)
            assert "/proc/no/such/dir" in error["message"], error

            assert (await command("quit"))["quit"]
            assert process.wait(timeout=10) == 0
        finally:
            if process.poll() is None:
                process.terminate()
            process.wait(timeout=10)
            for shot in sorted(workdir.rglob("*"), reverse=True):
                shot.rmdir() if shot.is_dir() else shot.unlink()
            workdir.rmdir()
    print("native_control_smoke: ok")


asyncio.run(run())
