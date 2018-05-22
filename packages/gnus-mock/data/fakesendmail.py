#!/usr/bin/env python

import argparse
import sys
import os
import datetime

incoming_dir = os.path.join(sys.path[0], "test/incoming/new")
message_f_name = "message"

# Arguments set up to match what message-send-mail-with-sendmail uses.
# Currently all these arguments are ignored except for the message
# itself.
parser = argparse.ArgumentParser(description='Pretend that we are sendmail.')
parser.add_argument('message', default=sys.stdin, nargs='?',
                    type=argparse.FileType("r"), help="Message to send")
parser.add_argument("-oi", nargs='?', const="nothing",
                    default="nothing", help="Extra args")
parser.add_argument("-oem", action='store_true', help="Report errors by mail")
parser.add_argument("-odb", action='store_true', help="Deliver in background")
parser.add_argument("-f", dest="from", help="From address")
parser.add_argument("-t", action='store_true',
                    help="Extract To address from message")

args = parser.parse_args()

counter = 0
while message_f_name in os.listdir(incoming_dir):
    message_f_name = "%s_%d" % (message_f_name, counter)
    counter += 1

with open(os.path.join(incoming_dir, message_f_name), "w") as message_file:
    message_file.write("Received: from Gnus mock fakemail program on %s\n" %
                       datetime.datetime.now().strftime("%c"))
    for line in args.message:
        message_file.write(line)
