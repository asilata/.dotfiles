#! /usr/bin/env python2
import os, re

def get_user(machine):
    authinfo = os.popen("gpg2 -dq ~/.authinfo.gpg").read()
    s = "machine %s login (?P<user>.*?) port (?:.*?) password (?P<pass>.*?)\n" % (machine)
    m = re.search(s, authinfo)
    return m.group('user')

def get_pass(machine):
    authinfo = os.popen("gpg2 -dq ~/.authinfo.gpg").read()
    s = "machine %s login (?P<user>.*?) port (?:.*?) password (?P<pass>.*?)\n" % (machine)
    m = re.search(s, authinfo)
    return m.group('pass')

def get_client_id():
    return os.popen("gpg2 -dq ~/.offlineimap/.offlineimap-client-id.gpg")

def get_client_secret():
    return os.popen("gpg2 -dq ~/.offlineimap/.offlineimap-client-secret.gpg")

def get_refresh_token():
    return os.popen("gpg2 -dq ~/.offlineimap/.offlineimap-refresh-token.gpg")
