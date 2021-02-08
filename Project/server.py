"""
Python server implementing asyncio
I used TA Kimmo's slides and the TA code help github repository to help code this project.
"""

import json
import argparse
import logging
import time
import sys
import asyncio
import aiohttp
import re


PRINT_FLAG = 0
OUTPUT_LOG_FLAG = 1
API_KEY = 'AIzaSyDW_dZ2AHURGjAl4ahleORvrTvGwEuZuso'

valid_server_names = {"Hill": 12120, "Jaquez": 12121, "Smith": 12122, "Campbell": 12123, "Singleton": 12124}

server_neighbors = {
    "Hill": ["Jaquez", "Smith"],
    "Jaquez": ["Hill", "Singleton"],
    "Smith": ["Hill", "Singleton", "Campbell"],
    "Campbell": ["Smith", "Singleton"],
    "Singleton": ["Campbell", "Jaquez", "Smith"]
}
localhost = '127.0.0.1'


class Server:
    def __init__(self, name, ip, port):
        self.name = name
        self.ip = ip
        self.port = port
        self.client_info = dict() 
        self.client_message = dict()
        logging.info("Initialize log file for server {}".format(name))


    async def xferMessageToServers(self, message):
        # send message to every server connected
        for neighbor in server_neighbors[self.name]:
            try:
                reader, writer = await asyncio.open_connection('127.0.0.1', valid_server_names[neighbor])
                outputLogMessage("{} send to {}: {}".format(self.name, neighbor, message))
                writer.write(message.encode())
                await writer.drain()
                outputLogMessage("Closing connection to {}".format(neighbor))
                writer.close()
                await writer.wait_closed()
            except:
                outputLogMessage("Error connecting to server {}".format(neighbor))


    async def getGoogleLocations(self, location, radius, upper_bound):
        async with aiohttp.ClientSession() as session:
            # need to construct arguments for api
            coordinates = self.getMessageCoordinates(location)
            if coordinates == None: 
                print("False coordinate format")
                sys.exit()
            outputLogMessage("Query at location {}".format(coordinates))
            url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={}&radius={}&key={}'.format(coordinates, radius, API_KEY)
            result = await self.getUrlResponse(session, url)
            result_object = json.loads(result)
            outputLogMessage("Successfully retrieved {} place(s) from API".format(len(result_object["results"])))
            if len(result_object["results"]) <= int(upper_bound):
                return result
            else:
                result_object["results"] = result_object["results"][0:int(upper_bound)]
                return json.dumps(result_object, sort_keys=True, indent=4)
             

    async def getUrlResponse(self, session, url):
        async with session.get(url) as response:
            return await response.text()

    def getMessageCoordinates(self, location):
        plus = location.rfind('+')
        minus = location.rfind('-')
        if plus != -1 and plus != 0:
            return "{},{}".format(location[0:plus], location[plus:])
        if minus != -1 and minus != 0:
            return "{},{}".format(location[0:minus], location[minus:])
        return None

    def processCommand_WHATSAT(self, strings):
        if not (testIfNumber(strings[2]) and testIfNumber(strings[3])):
            return False
        if int(strings[2]) > 50 or int(strings[2]) < 0:
            return False
        if int(strings[3]) > 20 or int(strings[3]) < 0:
            return False
        if strings[1] not in self.client_info:
            return False
        return True


    def processCommand_IAMAT(self, strings):
        temp = strings[2].replace('+', '-')
        nums = list(filter(None, temp.split('-')))
        if len(nums) != 2 or not (testIfNumber(nums[0]) and testIfNumber(nums[1])):
            return False
        if not testIfNumber(strings[3]):
            return False
        return True
    
    
    async def runServerForever(self):
        outputLogMessage('starting up {} server...'.format(self.name))
        server = await asyncio.start_server(self.processClientInput, self.ip, self.port)
        async with server:
            await server.serve_forever()
        outputLogMessage('{} server shutting down...'.format(self.name))
        server.close()

    async def processClientInput(self, reader, writer):
        while not reader.at_eof():
            data = await reader.readline()
            message = data.decode()
            if message == "": # ignore empty messages
                continue
            outputLogMessage("{} recieved: {}".format(self.name, message))
            strings = message.split()
            if len(strings) != 4:  # not a regular command
                if len(strings) == 6 and strings[0] == "AT":
                    outputLogMessage("Received propagated message...")
                    sendback_message = None
                    if strings[3] in self.client_info:
                        outputLogMessage("Already contain data for client {}".format(strings[3]))
                        if float(strings[5]) > self.client_info[strings[3]]: 
                            outputLogMessage("Updating data for client {}... and propagating new data...".format(strings[1]))
                            self.client_info[strings[3]] = float(strings[5])
                            self.client_message[strings[3]] = message
                            await self.xferMessageToServers(message)
                        else:
                            outputLogMessage("Received message already... Stop propagation.")
                            pass
                    else: 
                        outputLogMessage("New data for client {}... propagate new data...".format(strings[1]))
                        self.client_info[strings[3]] = float(strings[5])
                        self.client_message[strings[3]] = message
                        await self.xferMessageToServers(message)
                else: 
                    sendback_message = "? " + message
            elif strings[0] == "IAMAT":
                if self.processCommand_IAMAT(strings):
                    diff = time.time() - float(strings[3])
                    str_diff = ["", "+"][diff > 0] + str(diff)
                    sendback_message = "AT {} {} {} {} {}".format(self.name, str_diff, strings[1], strings[2], strings[3])
                    self.client_info[strings[1]] = float(strings[3])
                    self.client_message[strings[1]] = sendback_message
                    await self.xferMessageToServers(sendback_message)
                else:
                    sendback_message = "? " + message
            elif strings[0] == "WHATSAT":
                if self.processCommand_WHATSAT(strings):
                    location = self.client_message[strings[1]].split()[4]
                    radius = strings[2]
                    bound = strings[3]
                    places = await self.getGoogleLocations(location, radius, bound)
                    sendback_message = "{}\n{}\n\n".format(self.client_message[strings[1]], str(places).rstrip('\n'))
                else:
                    sendback_message = "? " + message
            else:
                sendback_messgae = "? " + message

            if sendback_message != None:
                outputLogMessage("Sending message back to client: {}".format(sendback_message))
                writer.write(sendback_message.encode())
                await writer.drain()
        outputLogMessage("close the client socket")
        writer.close()

def testIfNumber(string):
    try:
        float(string)
        return True
    except ValueError:
        return False

def outputLogMessage(msg):
    if OUTPUT_LOG_FLAG == 1:
        logging.info(msg)
    if PRINT_FLAG:
        print(msg)

def main():
    parser = argparse.ArgumentParser('CS131 Project Parser')
    parser.add_argument('server_name', type=str, help='required server name input')
    args = parser.parse_args()
    if not args.server_name in valid_server_names:
        print("Invalid Server Name {}".format(args.server_name))
        sys.exit()
    logging.basicConfig(filename="{}.log".format(args.server_name), format='%(levelname)s: %(message)s', filemode='w+', level=logging.INFO)
    server = Server(args.server_name, localhost, valid_server_names[args.server_name])
    try:
        asyncio.run(server.runServerForever())
    except KeyboardInterrupt:
        pass

if __name__ == '__main__':
    main()
