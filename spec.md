# Web Chat System Specification

## WebSocket Connection

- URL: `ws://localhost:8080/ws`

## Message Format

All messages are JSON objects with a `type` field indicating the message type.

## Client Actions

### 1. Login

Request:
```json
{
  "type": "login",
  "username": "Alice"
}
```

Possible Responses:
- Success:
  ```json
  {
    "type": "login_ack",
    "username": "Alice"
  }
  ```
- Error:
  ```json
  {
    "type": "error",
    "reason": "Username already taken"
  }
  ```

### 2. Join Room

Request:
```json
{
  "type": "join",
  "room": "room1"
}
```

Possible Responses:
- Success:
  ```json
  {
    "type": "join_ack",
    "room": "room1"
  }
  ```
- Error:
  ```json
  {
    "type": "error",
    "reason": "Not logged in"
  }
  ```
  or
  ```json
  {
    "type": "error",
    "reason": "Already in room"
  }
  ```

### 3. Leave Room

Request:
```json
{
  "type": "leave",
  "room": "room1"
}
```

Possible Responses:
- Success:
  ```json
  {
    "type": "leave_ack",
    "room": "room1"
  }
  ```
- Error:
  ```json
  {
    "type": "error",
    "reason": "Not logged in"
  }
  ```
  or
  ```json
  {
    "type": "error",
    "reason": "Not in room"
  }
  ```

### 4. Send Message

Request:
```json
{
  "type": "message",
  "room": "room1",
  "content": "Hello, everyone!"
}
```

Possible Responses:
- Success:
  ```json
  {
    "type": "message_ack"
  }
  ```
- Error:
  ```json
  {
    "type": "error",
    "reason": "Not logged in"
  }
  ```
  or
  ```json
  {
    "type": "error",
    "reason": "Not in room"
  }
  ```

### 5. Broadcast Message

Request:
```json
{
  "type": "broadcast",
  "content": "Hello, all rooms!"
}
```

Possible Responses:
- Success:
  ```json
  {
    "type": "broadcast_ack"
  }
  ```
- Error:
  ```json
  {
    "type": "error",
    "reason": "Not logged in"
  }
  ```

### 6. Get All Rooms

Request:
```json
{
  "type": "get_all_rooms"
}
```

Response:
```json
{
  "type": "all_rooms",
  "rooms": ["room1", "room2", "room3"]
}
```

### 7. Get Users in Room

Request:
```json
{
  "type": "get_users_in_room",
  "room": "room1"
}
```

Possible Responses:
- Success:
  ```json
  {
    "type": "users_in_room",
    "room": "room1",
    "users": ["Alice", "Bob", "Charlie"]
  }
  ```
- Error:
  ```json
  {
    "type": "error",
    "reason": "Room not found"
  }
  ```

### 8. Get Server Info

Request:
```json
{
  "type": "get_server_info"
}
```

Response:
```json
{
  "type": "server_info",
  "info": {
    "total_rooms": 3,
    "total_users": 10,
    "avg_users_per_room": "3.33",
    "largest_room": {
      "name": "room1",
      "size": 5
    }
  }
}
```

## Server-Initiated Messages

### 1. Ping (keep-alive)

Server sends:
```json
{
  "type": "ping"
}
```

Client should respond with:
```json
{
  "type": "pong"
}
```

### 2. User Joined Notification

```json
{
  "type": "user_joined",
  "room": "room1",
  "username": "Alice"
}
```

### 3. User Left Notification

```json
{
  "type": "user_left",
  "room": "room1",
  "username": "Alice"
}
```

### 4. Incoming Message

```json
{
  "type": "message",
  "room": "room1",
  "from": "Bob",
  "content": "Hello, everyone!"
}
```

## Error Handling

All errors are returned in the following format:
```json
{
  "type": "error",
  "reason": "Error message"
}
```

Possible error reasons:
- "Not logged in"
- "Username already taken"
- "Already in room"
- "Not in room"
- "Room not found"
- "Invalid message format"
- "Unknown message type"
- "Internal error"

## UI Implementation Guidelines

1. Connection:
   - Provide input for WebSocket URL and a connect button.
   - Show connection status (connected/disconnected).

2. Login:
   - Username input field and login button.
   - Display current username when logged in.

3. Room Management:
   - List of available rooms (updateable via getAllRooms()).
   - Join room button next to each room.
   - Create new room functionality (joins automatically if successful).

4. Chat Interface:
   - Display current room name.
   - Message history area.
   - Input field for new messages.
   - Send button.
   - Leave room button.

5. User List:
   - Sidebar showing users in the current room.
   - Update when users join or leave.

6. Broadcast:
   - Separate input and button for broadcasting messages.

7. Server Info:
   - Button to fetch and display server information.

8. Error Handling:
   - Display error messages in a noticeable area (e.g., toast notifications).

9. Responsiveness:
   - Update UI immediately on user actions.
   - Revert changes if server responds with an error.

Remember to handle the WebSocket's ping messages by responding with a pong to keep the connection alive.