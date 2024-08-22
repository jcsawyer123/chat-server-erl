import asyncio
from typing import List, Set, Dict, Coroutine
from ui import ChatUI
from websocket_client import WebSocketClient
from message_handler import MessageHandler

class ChatApp:
    def __init__(self, master):
        self.master = master
        self.master.title("WebSocket Chat App")
        self.master.geometry("800x600")

        self.ui = ChatUI(master, self)
        self.websocket_client = WebSocketClient('ws://localhost:8080/ws')
        self.message_handler = MessageHandler(self)

        self.username: str = None
        self.joined_rooms: Set[str] = set()
        self.current_room: str = None
        self.room_messages: Dict[str, List[str]] = {}
        
        self.refresh_interval = 30000  # 30 seconds
        self.refresh_task = None
        
    @staticmethod
    def validate_string_input(input_value, max_length=100):
        if input_value is None:
            return None
        validated_input = str(input_value).strip()
        return validated_input[:max_length] if validated_input else None
    
    async def connect(self):
        await self.websocket_client.connect()
        self.master.after(100, self.start_receiving)
        await self.login()
        
    def schedule_task(self, coro: Coroutine):
        asyncio.create_task(coro)
        
    async def login(self):
        while True:
            username = self.ui.ask_username()
            username = self.validate_string_input(username, max_length=30)
            if not username:
                self.master.quit()  # Exit the application if login is cancelled or empty
                return
            self.username = username

            await self.websocket_client.send_message({
                "type": "login",
                "username": username
            })
            
            response = await self.websocket_client.get_message()
            if response is None:
                self.ui.show_error("Connection closed during login")
                self.master.quit()
                return
            
            if response['type'] == 'login_ack':
                self.ui.show_info(f"Logged in as {self.username}")
                await self.get_all_rooms()
                self.start_periodic_refresh()
                break
            elif response['type'] == 'error' and 'Username already taken' in response['reason']:
                self.ui.show_error("Username already taken. Please choose another.")
            else:
                self.ui.show_error(f"Login failed: {response.get('reason', 'Unknown error')}")
                self.master.quit()
                return

    def start_receiving(self):
        asyncio.create_task(self.receive_messages())

    async def receive_messages(self):
        while True:
            message = await self.websocket_client.get_message()
            if message is None:
                print("Connection closed")
                break
            await self.message_handler.handle_message(message)
        
    async def send_message(self, message_type: str, **kwargs):
        message = {"type": message_type, **kwargs}
        await self.websocket_client.send_message(message)

    async def get_all_rooms(self):
        await self.send_message("get_all_rooms")

    async def join_room(self, room: str):
        if room not in self.joined_rooms:
            await self.send_message("join", room=room)
            self.joined_rooms.add(room)
            if room not in self.room_messages:
                self.room_messages[room] = []
            await self.update_ui()
        self.select_room(room)

    def select_room(self, room: str):
        self.current_room = room
        self.ui.update_chat(self.room_messages.get(room, []))

    async def leave_room(self, room: str):
        if room in self.joined_rooms:
            await self.send_message("leave", room=room)
            self.joined_rooms.remove(room)
            if room in self.room_messages:
                del self.room_messages[room]
            if self.current_room == room:
                self.current_room = None
            await self.update_ui()
            
    async def update_room_list(self, rooms: List[str]):
        all_rooms = set(rooms)
        self.joined_rooms &= all_rooms
        self.room_messages = {room: self.room_messages.get(room, []) for room in all_rooms}
        await self.update_ui()

    def add_message_to_room(self, room: str, message: str):
        if room in self.room_messages:
            self.room_messages[room].append(message)
            if room == self.current_room:
                self.ui.update_chat(self.room_messages[room])
                
    def start_periodic_refresh(self):
        self.refresh_task = self.master.after(self.refresh_interval, self.periodic_refresh)

    def periodic_refresh(self):
        self.schedule_task(self.get_all_rooms())
        self.refresh_task = self.master.after(self.refresh_interval, self.periodic_refresh)
        
    async def update_ui(self):
        self.ui.update_room_list(list(self.room_messages.keys()), self.joined_rooms)
        if self.current_room:
            self.ui.update_chat(self.room_messages[self.current_room])

    def show_error(self, error: str):
        self.ui.show_error(error)

    def show_info(self, info: str):
        self.ui.show_info(info)
        
    async def close(self):
        if self.refresh_task:
            self.master.after_cancel(self.refresh_task)
        await self.websocket_client.close()