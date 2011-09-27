#!/usr/bin/env python
#-*- coding: ISO-8859-1 -*-
"""
File: erl_client.py
Author: Valentin Kuznetsov <vkuznet@gmail.com>
Description: A python client which access Erland bapp_server
"""
import os
import sys
import json
import pyerl
import types
import socket
from optparse import OptionParser
from pyerl_utils.utils import convert, convert_str, convert_list_to_str, ErlTuple, ErlGen

if sys.version_info < (2, 6):
    raise Exception("DAS requires python 2.6 or greater")

class DASOptionParser: 
    """
    DAS cli option parser
    """
    def __init__(self):
        status_usage  = "Obtain status on a server. Acceptable formats:\n"
        status_usage += "--status=guid,123\n"
        status_usage += "--status=pid,mynode,0.718.0\n"
        status_usage += "--status=node,mynode\n"
        test_dir = '/'.join(__file__.split('/')[:-1])
        if  test_dir == '.':
            test_dir = os.getcwd() + "/input_files"
        self.parser = OptionParser()
        self.parser.add_option("--cmd", action="store", type="string", 
                                          default="", dest="command",
             help="specify command to run")
        self.parser.add_option("--dir", action="store", type="string", 
                                          default=test_dir, dest="dir",
             help="specify input dir")
        self.parser.add_option("--host", action="store", type="string", 
                                          default="localhost", dest="host",
             help="specify host name of bapp server")
        self.parser.add_option("--cookie", action="store", type="string", 
                                          default="cookiestring", dest="cookie",
             help="specify cookie string for bapp server")
        self.parser.add_option("--test", action="store_true", dest="test", default=True,
             help="test mode")
        self.parser.add_option("--status", action="store", type="string", 
                                          default="", dest="status",
             help=status_usage)
    def getOpt(self):
        """
        Returns parse list of options
        """
        return self.parser.parse_args()

def test_black_box(sock, server, icmd, idir):
    api = "handle_call"
    atom = pyerl.mk_atom("process")
    cmd  = pyerl.mk_string(icmd)
    name = pyerl.mk_string(idir)
    tup = pyerl.mk_tuple((atom, cmd, name))
    pid = pyerl.mk_pid("node", 1, 2, 3)
    state = pyerl.mk_atom("atom") # passed state to server is irrelevant in this call
    args = pyerl.mk_list([tup, pid, state])
    eterm = pyerl.rpc(sock, server, api, args)
    return eterm

def get_status(sock, server, node, uinput):
    what  = uinput[0]
    api   = "handle_call"
    status= pyerl.mk_atom("status")
    if  what == 'node':
        value = uinput[-1]
        val = pyerl.mk_atom(value)
    elif what == 'guid':
        value = uinput[-1]
        val = pyerl.mk_int(int(value))
    elif what == 'pid':
        rnode = uinput[1] # remote node
        pid   = uinput[2] # remote pid
        nodename = pyerl.mk_atom(rnode)
        ppid = pyerl.mk_string(pid)
        val   = pyerl.mk_tuple((nodename, ppid))
    else:
        raise Exception('Unknown message: %s' %  what)
    what  = pyerl.mk_atom(what)
    pid   = pyerl.mk_pid("node", 1, 2, 3)
    state = pyerl.mk_atom("atom") # passed state to server is irrelevant in this call
    tup   = pyerl.mk_tuple((status, what, val))
    args  = pyerl.mk_list([tup, pid, state])
    eterm = pyerl.rpc(sock, server, api, args)
    return eterm

def connect(host, cookie, cmd, idir, status=''):
    # set communication channel to our Erlang node, user must specify host, node
    # name, cookie.
    addr = socket.gethostbyaddr(host)
    hostname, aliaslist, addresslist = socket.gethostbyaddr(host)
    addr = addresslist[0]
    name = "mynode"
    node = name + "@" + hostname
    # initialize the erl_connect module, see http://www.erlang.org/doc/man/erl_connect.html
    ret  = pyerl.connect_xinit(host, name, node, addr, cookie, 1)
    sock = pyerl.xconnect(addr, name)
    if  sock < 0: # fail to connect
        print "Fail to connect to Erlang node"
        sys.exit(0)
#    print "connect to node=%s, addr=%s, sock=%s" % (node, addr, sock)

    # call test code
    server = "bapp_server"
    if  status:
        eterm = get_status(sock, server, name, status)
    else:
        eterm = test_black_box(sock, server, cmd, idir)
    print "server reply:", eterm

    # close connection to our server
    pyerl.close_connection(sock)

if __name__ == '__main__':
    optManager  = DASOptionParser()
    (opts, args) = optManager.getOpt()

    cookie = opts.cookie
    host = opts.host
    cmd  = opts.command
    idir = opts.dir

    if  opts.test and not opts.command:
        test_dir = '/'.join(__file__.split('/')[:-1])
        if  test_dir == '.':
            test_dir = os.getcwd()
        cmd = '%s/%s' % (test_dir, 'test.py')
    print "Invoke host: %s, cmd: %s, idir: %s" % (host, cmd, idir)
    if  opts.status:
        connect(host, cookie, cmd, idir, opts.status.split(','))
    else:
        connect(host, cookie, cmd, idir)
