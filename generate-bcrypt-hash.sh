#!/bin/bash
#
# Generate bcrypt password hashes for ShinyProxy authentication.
#
# This script prompts for a password and outputs a bcrypt hash that can be
# used in application.yml. ShinyProxy uses bcrypt with 10 rounds by default.
#
# Usage:
#     bash generate-bcrypt-hash.sh
#
# Requirements:
#     - htpasswd utility (usually available via apache2-utils or httpd-tools)

set -e

echo "===================================================================="
echo "ShinyProxy Bcrypt Password Hash Generator"
echo "===================================================================="
echo ""

# Check if htpasswd is available
if ! command -v htpasswd &> /dev/null; then
    echo "Error: htpasswd command not found."
    echo ""
    echo "Install it with:"
    echo "  - Debian/Ubuntu: apt-get install apache2-utils"
    echo "  - RHEL/CentOS:   yum install httpd-tools"
    echo "  - Alpine:        apk add apache2-utils"
    exit 1
fi

# Get username (optional, for display purposes)
read -p "Enter username (optional, for reference): " username
username=${username:-user}

# Get password securely
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

# Generate hash using htpasswd with bcrypt
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
