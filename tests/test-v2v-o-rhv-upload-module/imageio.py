#!/usr/bin/env python3
# -*- python -*-
# Copyright (C) 2018-2021 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

# Fake imageio web server used as a test harness.
# See v2v/test-v2v-o-rhv-upload.sh

import sys
import threading
from http.server import HTTPServer, BaseHTTPRequestHandler

class RequestHandler(BaseHTTPRequestHandler):
    protocol_version = 'HTTP/1.1'

    def do_OPTIONS(self):
        self.discard_request()

        # Advertize only flush and zero support.
        content = b'''{ "features": [ "flush", "zero" ] }'''
        length = len(content)

        self.send_response(200)
        self.send_header("Content-type", "application/json; charset=UTF-8")
        self.send_header("Content-Length", length)
        self.end_headers()
        self.wfile.write(content)

    # eg. zero request.  Just ignore it.
    def do_PATCH(self):
        self.discard_request()
        self.send_response(200)
        self.send_header("Content-Length", "0")
        self.end_headers()

    # Flush request.  Ignore it.
    def do_PUT(self):
        self.discard_request()
        self.send_response(200)
        self.send_header("Content-Length", "0")
        self.end_headers()

    def discard_request(self):
        length = self.headers.get('Content-Length')
        if length:
            length = int(length)
            content = self.rfile.read(length)

server_address = ("", 0)
# XXX This should test HTTPS, not HTTP, because we are testing a
# different path through the main code.
httpd = HTTPServer(server_address, RequestHandler)
imageio_port = httpd.server_address[1]

print("port: %d" % imageio_port)
sys.stdout.flush()

httpd.serve_forever()
