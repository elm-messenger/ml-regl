#!/usr/bin/env python3
"""Exercise the native ml-regl control client over a real WebSocket."""

from __future__ import annotations

import asyncio
import json
import os
from pathlib import Path
import subprocess
import tempfile

import websockets


async def run() -> None:
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
        root = Path(__file__).resolve().parents[1]
        binary = root / "_build/default/test/test_fps_smoke_desktop.exe"
        screenshot = Path(tempfile.gettempdir()) / "ml-regl-native-control.bmp"
        screenshot.unlink(missing_ok=True)
        env = os.environ.copy()
        env["DECLGL_DEBUG"] = "1"
        env["DECLGL_CONTROL_URL"] = f"ws://127.0.0.1:{port}"
        process = subprocess.Popen(
            [str(binary)],
            cwd=root / "test",
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

            async def command(method, params=None, command_id=1):
                message = {"method": method, "id": command_id}
                if params is not None:
                    message["params"] = params
                await socket.send(json.dumps(message))
                response = await next_message(
                    lambda item: item.get("type") == "response"
                    and item.get("id") == command_id
                )
                assert response["ok"], response
                return response["result"]

            assert (await command("pause", command_id=1))["paused"]
            state = await command("get_state", command_id=2)
            assert state["paused"] is True
            assert (await command("step", {"frames": 1, "dt_ms": 10}, 3))["queued"] == 1
            await next_message(lambda item: item.get("type") == "frame")
            assert (await command("input", {"kind": "mouse_move", "x": 20, "y": 30}, 4))["delivered"]
            tree = await command("get_render_tree", command_id=5)
            assert tree["available"] is True
            capture = await command("screenshot", {"path": str(screenshot)}, 6)
            assert capture["path"] == str(screenshot)
            assert screenshot.is_file() and screenshot.stat().st_size > 0
            assert (await command("quit", command_id=7))["quit"]

            assert process.wait(timeout=10) == 0
        finally:
            if process.poll() is None:
                process.terminate()
            process.wait(timeout=10)
            screenshot.unlink(missing_ok=True)


asyncio.run(run())
