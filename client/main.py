import asyncio
import tkinter as tk
from chat_app import ChatApp

async def main():
    root = tk.Tk()
    app = ChatApp(root)
    
    try:
        await app.connect()
        while True:
            root.update()
            await asyncio.sleep(0.01)
    except tk.TclError:
        print("Window closed")
    finally:
        await app.close()

if __name__ == "__main__":
    asyncio.run(main())