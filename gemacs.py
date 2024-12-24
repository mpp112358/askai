#!/usr/bin/env python3

import pathlib
import textwrap
import json

import google.generativeai as genai

import markdown

with open("config.json") as configFile:
    config = json.load(configFile)
    GOOGLE_API_KEY = config["api_key"]

genai.configure(api_key=GOOGLE_API_KEY)

model = genai.GenerativeModel("gemini-1.5-flash")

chat = model.start_chat(history=[])

message = input()
response = chat.send_message(message)
print(response.text)
