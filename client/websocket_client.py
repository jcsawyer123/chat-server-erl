import json
import websockets
import asyncio
from typing import Union, Dict, Any

class WebSocketClient:
    def __init__(self, url: str):
        self.url = url
        self.websocket = None
        self.receive_task = None
        self.message_queue = asyncio.Queue()

    async def connect(self):
        try:
            self.websocket = await websockets.connect(self.url)
            self.receive_task = asyncio.create_task(self._receive_messages())
        except Exception as e:
            raise ConnectionError(f"Failed to connect to WebSocket server: {str(e)}")

    async def send_message(self, message: Union[str, Dict[str, Any]]):
        if not self.websocket:
            raise ConnectionError("WebSocket is not connected")
        
        if isinstance(message, dict):
            message = json.dumps(message)
        
        await self.websocket.send(message)
                
    async def get_message(self) -> Union[Dict[str, Any], None]:
        return await self.message_queue.get()

    async def _receive_messages(self):
        try:
            async for message in self.websocket:
                print(f"WS: Received message: {message}")
                await self.message_queue.put(json.loads(message))
        except websockets.exceptions.ConnectionClosed:
            print("WebSocket connection closed")
        finally:
            await self.message_queue.put(None)  # Signal that the connection is closed

    async def close(self):
        if self.websocket:
            await self.websocket.close()
        if self.receive_task:
            self.receive_task.cancel()
            try:
                await self.receive_task
            except asyncio.CancelledError:
                pass