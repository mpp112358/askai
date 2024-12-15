#!/usr/bin/env python3

import pathlib
import textwrap
from cryptography.fernet import Fernet

import google.generativeai as genai

import markdown

with open(".secrets") as secrets:
    GOOGLE_API_KEY = secrets.readline().rstrip()

genai.configure(api_key=GOOGLE_API_KEY)

model = genai.GenerativeModel("gemini-1.5-flash")

chat = model.start_chat(history=[])

message = ""

while not message == "bye":
    message = input("> ")
    response = chat.send_message(message)
    print(response.text)
