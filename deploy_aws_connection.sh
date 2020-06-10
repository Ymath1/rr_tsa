#!/bin/bash
chmod 400 rr_project.pem
ssh -i "rr_project.pem" ubuntu@ec2-100-25-188-51.compute-1.amazonaws.com;bash
