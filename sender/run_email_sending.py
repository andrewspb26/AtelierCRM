# -*- coding: utf-8 -*-

from imp import reload
import db_status_monitor

while True:
        time.sleep(600)
        reload(db_status_monitor)
