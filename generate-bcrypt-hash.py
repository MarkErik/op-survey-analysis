#!/usr/bin/env python

import getpass
import sys

try:
    import bcrypt
except ImportError:
    print("Error: bcrypt module not found.")
    print("Install it with: pip install bcrypt")
    sys.exit(1)


def generate_bcrypt_hash(password: str, rounds: int = 10) -> str:

    password_bytes = password.encode('utf-8')

    salt = bcrypt.gensalt(rounds=rounds)
    hashed = bcrypt.hashpw(password_bytes, salt)

    return hashed.decode('utf-8')


def main():
    print("=" * 60)
    print("ShinyProxy Bcrypt Password Hash Generator")
    print("=" * 60)
    print()

    username = input("Enter username (optional, for reference): ").strip() or "user"

    password = getpass.getpass(f"Enter password for '{username}': ")

    password_confirm = getpass.getpass("Confirm password: ")

    if password != password_confirm:
        print("\nError: Passwords do not match!")
        sys.exit(1)

    if not password:
        print("\nError: Password cannot be empty!")
        sys.exit(1)

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
