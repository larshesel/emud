#!/bin/bash
erl -pa ./ebin -sname client$1 -setcookie 'emud' -remsh emud@stella
