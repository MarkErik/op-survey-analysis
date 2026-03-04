#!/bin/bash

set -e

echo "===================================================================="
echo "ShinyProxy Bcrypt Password Hash Generator"
echo "===================================================================="
echo ""

if ! command -v htpasswd &> /dev/null; then
    echo "Error: htpasswd command not found."
    echo ""
    echo "Install it with:"
    echo "  - Debian/Ubuntu: apt-get install apache2-utils"
    echo "  - RHEL/CentOS:   yum install httpd-tools"
    echo "  - Alpine:        apk add apache2-utils"
    exit 1
fi

read -p "Enter username (optional, for reference): " username
username=${username:-user}

read -s -p "Enter password for '$username': " password
echo ""
read -s -p "Confirm password: " password_confirm
echo ""

if [ "$password" != "$password_confirm" ]; then
    echo ""
    echo "Error: Passwords do not match!"
    exit 1
fi

if [ -z "$password" ]; then
    echo ""
    echo "Error: Password cannot be empty!"
    exit 1
fi

echo ""
echo "Generating bcrypt hash..."
hashed_password=$(htpasswd -nbB "$username" "$password" | cut -d: -f2)

echo ""
echo "===================================================================="
echo "SUCCESS!"
echo "===================================================================="
echo ""
echo "Username: $username"
echo "Bcrypt Hash: $hashed_password"
echo ""
echo "Copy the hash above and paste it into application.yml:"
echo ""
echo "  users:"
echo "    - name: $username"
echo "      password: $hashed_password"
echo "      groups: your-group"
echo ""
