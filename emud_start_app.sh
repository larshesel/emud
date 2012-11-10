#!/bin/bash
erl -pa ./ebin  -sname emud -setcookie 'emud' -s emud start -s mymap init -s
