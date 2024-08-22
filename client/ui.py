import tkinter as tk
from tkinter import ttk, messagebox, simpledialog
from typing import List, Set

class ChatUI:
    def __init__(self, master, app):
        self.master = master
        self.app = app
        self.create_widgets()

    def create_widgets(self):
        self.main_frame = ttk.Frame(self.master, padding="3 3 12 12")
        self.main_frame.grid(column=0, row=0, sticky=(tk.N, tk.W, tk.E, tk.S))
        self.master.columnconfigure(0, weight=1)
        self.master.rowconfigure(0, weight=1)

        self.create_sidebar()
        self.create_chat_area()
        self.create_message_input()

        self.configure_weights()

    def create_sidebar(self):
        self.sidebar = ttk.Frame(self.main_frame, width=200)
        self.sidebar.grid(column=0, row=0, rowspan=2, sticky=(tk.N, tk.S, tk.W))

        self.room_list = ttk.Treeview(self.sidebar, columns=('room',), show='tree')
        self.room_list.heading('#0', text='Rooms')
        self.room_list.column('#0', width=180)
        self.room_list.grid(column=0, row=0, sticky=(tk.N, tk.S, tk.E, tk.W))
        self.room_list.bind('<<TreeviewSelect>>', self.on_room_select)
        self.room_list.bind('<Double-1>', self.on_room_double_click)
        
        buttons = [
            ("Join Room", self.join_room),
            ("Leave Room", self.leave_room),
            ("Create Room", self.create_room),
            ("Refresh Rooms", self.refresh_rooms)
        ]

        for i, (text, command) in enumerate(buttons, start=1):
            ttk.Button(self.sidebar, text=text, command=command).grid(column=0, row=i, sticky=(tk.E, tk.W))

    def create_chat_area(self):
        self.chat_frame = ttk.Frame(self.main_frame)
        self.chat_frame.grid(column=1, row=0, sticky=(tk.N, tk.S, tk.E, tk.W))

        self.chat_text = tk.Text(self.chat_frame, wrap=tk.WORD, state='disabled')
        self.chat_text.grid(column=0, row=0, sticky=(tk.N, tk.S, tk.E, tk.W))

        self.scrollbar = ttk.Scrollbar(self.chat_frame, orient='vertical', command=self.chat_text.yview)
        self.scrollbar.grid(column=1, row=0, sticky=(tk.N, tk.S))
        self.chat_text['yscrollcommand'] = self.scrollbar.set

    def create_message_input(self):
        self.input_frame = ttk.Frame(self.main_frame)
        self.input_frame.grid(column=1, row=1, sticky=(tk.E, tk.W))

        self.message_entry = ttk.Entry(self.input_frame, width=50)
        self.message_entry.grid(column=0, row=0, sticky=(tk.E, tk.W))

        ttk.Button(self.input_frame, text="Send", command=self.send_message).grid(column=1, row=0)
        ttk.Button(self.input_frame, text="Broadcast", command=self.broadcast_message).grid(column=2, row=0)

    def configure_weights(self):
        self.main_frame.columnconfigure(1, weight=1)
        self.main_frame.rowconfigure(0, weight=1)
        self.chat_frame.columnconfigure(0, weight=1)
        self.chat_frame.rowconfigure(0, weight=1)
        self.input_frame.columnconfigure(0, weight=1)

    def on_room_select(self, event):
        selected_items = self.room_list.selection()
        if selected_items:
            room = self.room_list.item(selected_items[0])['values'][0]
            self.app.select_room(room)

    def on_room_double_click(self, event):
        selected_items = self.room_list.selection()
        if selected_items:
            item = selected_items[0]
            room = str(self.room_list.item(item)['values'][0])
            if room not in self.app.joined_rooms:
                self.join_room(room)
            else:
                self.app.select_room(room)
                
    def join_room(self, room=None):
        if room is None:
            room = simpledialog.askstring("Join Room", "Enter room name:")
        if room:
            self.app.schedule_task(self.app.join_room(room))

    def leave_room(self):
        if self.app.current_room:
            self.app.schedule_task(self.app.leave_room(self.app.current_room))

    def create_room(self):
        room = simpledialog.askstring("Create Room", "Enter new room name:")
        if room:
            self.app.schedule_task(self.app.join_room(room))

    def send_message(self):
        message = self.message_entry.get()
        if message and self.app.current_room:
            self.app.schedule_task(self.app.send_message("message", room=self.app.current_room, content=message))
            self.message_entry.delete(0, tk.END)

    def broadcast_message(self):
        message = self.message_entry.get()
        if message:
            self.app.schedule_task(self.app.send_message("broadcast", content=message))
            self.message_entry.delete(0, tk.END)

    def refresh_rooms(self):
        self.app.schedule_task(self.app.get_all_rooms())

    def ask_username(self):
        return simpledialog.askstring("Login", "Enter your username:", parent=self.master)

    def update_chat(self, messages: List[str]):
        self.chat_text.config(state='normal')
        self.chat_text.delete('1.0', tk.END)
        for message in messages:
            self.chat_text.insert(tk.END, message + '\n')
        self.chat_text.config(state='disabled')
        self.chat_text.see(tk.END)
        
    def update_room_list(self, rooms: List[str], joined_rooms: Set[str]):
        self.room_list.delete(*self.room_list.get_children())
        for room in rooms:
            icon = '✓' if room in joined_rooms else ' '
            self.room_list.insert('', 'end', values=(room,), text=f"{icon} {room}")

    def show_error(self, error: str):
        messagebox.showerror("Error", error, parent=self.master)

    def show_info(self, info: str):
        messagebox.showinfo("Information", info, parent=self.master)