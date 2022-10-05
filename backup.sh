#!/bin/bash

git add . && git commit -m "backup $(date)" && git push -u origin master
