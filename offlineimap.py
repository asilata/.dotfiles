#! /usr/bin/env python2
from subprocess import check_output
import os

def get_pass(acc):
    if acc == "ANU":
        return check_output("gpg2 -dq ~/.mutt/anupassword.gpg", shell=True).strip("\n")

def get_user(acc):
    if acc == "ANU":
        with open(os.path.expanduser("~/.mutt/anuuser")) as f:
            return f.read().strip("\n")
