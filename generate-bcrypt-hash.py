#!/usr/bin/env python3
"""
Generate bcrypt password hashes for ShinyProxy authentication.

This script prompts for a password and outputs a bcrypt hash that can be
used in application.yml. ShinyProxy uses bcrypt with 10 rounds by default.

Usage:
    python3 generate-bcrypt-hash.py

Requirements:
    pip install bcrypt
"""

import getpass
import sys

try:
    import bcrypt
except ImportError:
    print("Error: bcrypt module not found.")
    print("Install it with: pip install bcrypt")
    sys.exit(1)


def generate_bcrypt_hash(password: str, rounds: int = 10) -> str:
    """
    Generate a bcrypt hash for the given password.

    Args:
        password: The plaintext password to hash
        rounds: Number of bcrypt rounds (default: 10, matches ShinyProxy)

    Returns:
        The bcrypt hash as a string
    """
    # Convert password to bytes
    password_bytes = password.encode('utf-8')

    # Generate salt and hash
    salt = bcrypt.gensalt(rounds=rounds)
    hashed = bcrypt.hashpw(password_bytes, salt)

    # Return as string (decode bytes)
    return hashed.decode('utf-8')


def main():
    print("=" * 60)
    print("ShinyProxy Bcrypt Password Hash Generator")
    print("=" * 60)
    print()

    # Get username (optional, for display purposes)
    username = input("Enter username (optional, for reference): ").strip() or "user"

    # Get password securely
    password = getpass.getpass(f"Enter password for '{username}': ")

    # Confirm password
    password_confirm = getpass.getpass("Confirm password: ")

    if password != password_confirm:
        print("\nError: Passwords do not match!")
        sys.exit(1)

    if not password:
        print("\nError: Password cannot be empty!")
        sys.exit(1)

    # Generate hash
    print("\nGenerating bcrypt hash...")
    hashed_password = generate_bcrypt_hash(password)

    print()
    print("=" * 60)
    print("SUCCESS!")
    print("=" * 60)
    print()
    print(f"Username: {username}")
    print(f"Bcrypt Hash: {hashed_password}")
    print()
    print("Copy the hash above and paste it into application.yml:")
    print()
    print("  users:")
    print(f"    - name: {username}")
    print(f"      password: {hashed_password}")
    print("      groups: your-group")
    print()


if __name__ == "__main__":
    main()
