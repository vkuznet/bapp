#!/usr/bin/env python

import os
import time
import sys

filename = '%s.out' % sys.argv[1]
print os.getcwd(), filename
time.sleep(5)
fds = open(filename, 'w')
fds.write('test')
fds.write('time=%s' % time.time())
fds.close()
