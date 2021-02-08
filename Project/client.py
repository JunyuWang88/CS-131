"""
I used TA Kimmo's slides to help me on this project.
"""
import asyncio
import argparse
import sys
import re

valid_server_names = {"Hill": 12120, "Jaquez": 12121, "Smith": 12122, "Campbell": 12123, "Singleton": 12124}
localhost = '127.0.0.1'


class Client:
    def __init__(self, ip='127.0.0.1', port=8888, name='client', message_max_length=1e6):
        self.ip = ip
        self.port = port
        self.name = name
        self.message_max_length = int(message_max_length)

    async def sendClientMessage(self, message):
        reader, writer = await asyncio.open_connection(self.ip, self.port)
        print('{} send: {}'.format(self.name, message))
        writer.write(message.encode())

        data = await reader.read(self.message_max_length)
        print('{} received: {}'.format(self.name, data.decode()))

        print('close the socket')
        writer.close()

    def transmitMessages(self):
        while True:
            message = input("Please input the next message to send: ")
            if message in ['quit', 'exit', ':q', 'exit;', 'quit;', 'exit()', '(exit)']:
                break
            else:
                message += "\n"
                asyncio.run(self.sendClientMessage(message))


if __name__ == '__main__':
    input_parser = argparse.ArgumentParser('Client Argument Parser')
    input_parser.add_argument('server_name', type=str, help='required server name that you want to connect to input')
    input_args = input_parser.parse_args()
    if not input_args.server_name in valid_server_names:
        print("Invalid Server Name: {}".format(input_args.server_name))
        sys.exit()
    client = Client(localhost, valid_server_names[input_args.server_name])
    client.transmitMessages()

