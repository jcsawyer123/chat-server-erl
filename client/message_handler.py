from typing import Dict, Any

class MessageHandler:
    def __init__(self, app):
        self.app = app
        self.handlers = {
            'join_ack': self._handle_join_ack,
            'leave_ack': self._handle_leave_ack,
            'message': self._handle_message,
            'user_joined': self._handle_user_joined,
            'user_left': self._handle_user_left,
            'all_rooms': self._handle_all_rooms,
            'error': self._handle_error,
            'server_info': self._handle_server_info
        }

    async def handle_message(self, data: Dict[str, Any]):
        handler = self.handlers.get(data['type'])
        if handler:
            await handler(data)
        else:
            print(f"Unknown message type: {data['type']}")

    async def _handle_join_ack(self, data):
        room = data['room']
        self.app.joined_rooms.add(room)
        self.app.add_message_to_room(room, f"You joined room: {room}")
        await self.app.update_ui()

    async def _handle_leave_ack(self, data):
        room = data['room']
        if room in self.app.joined_rooms:
            self.app.joined_rooms.remove(room)
        self.app.add_message_to_room(room, f"You left room: {room}")
        if room in self.app.room_messages:
            del self.app.room_messages[room]
        await self.app.update_ui()

    async def _handle_message(self, data):
        room = data['room']
        message = f"{data['from']}: {data['content']}"
        self.app.add_message_to_room(room, message)

    async def _handle_user_joined(self, data):
        room = data['room']
        self.app.add_message_to_room(room, f"{data['username']} joined {room}")

    async def _handle_user_left(self, data):
        room = data['room']
        self.app.add_message_to_room(room, f"{data['username']} left {room}")

    async def _handle_all_rooms(self, data):
        await self.app.update_room_list(data['rooms'])

    async def _handle_error(self, data):
        self.app.ui.show_error(data['reason'])

    async def _handle_server_info(self, data):
        self.show_server_info(data['info'])
    
    def show_server_info(self, info):
        info_str = f"Total Rooms: {info['total_rooms']}\n"
        info_str += f"Total Users: {info['total_users']}\n"
        info_str += f"Average Users per Room: {info['avg_users_per_room']}\n"
        info_str += f"Largest Room: {info['largest_room']['name']} (Size: {info['largest_room']['size']})"
        self.app.show_info(info_str)