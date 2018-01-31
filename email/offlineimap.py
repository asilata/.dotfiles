#! /usr/bin/env python2
import os, re

def get_user(machine):
    authinfo = os.popen("gpg -dq ~/.authinfo.gpg", 'r').read()
    s = "machine %s login (?P<user>.*?) port (?:.*?) password (?P<pass>.*?)\n" % (machine)
    m = re.search(s, authinfo)
    return m.group('user')

def get_pass(machine):
    authinfo = os.popen("gpg -dq ~/.authinfo.gpg").read()
    s = "machine %s login (?P<user>.*?) port (?:.*?) password (?P<pass>.*?)\n" % (machine)
    m = re.search(s, authinfo)
    return m.group('pass')

