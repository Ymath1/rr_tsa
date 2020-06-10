#!/bin/bash

scp -i "rr_project.pem" "app.R" ubuntu@ec2-100-25-188-51.compute-1.amazonaws.com:/srv/shiny-server/myapp;bash
