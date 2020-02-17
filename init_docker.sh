#!/bin/sh
SERVER_MODE=${SERVER_MODE:-attach}
SERVER_NAME=${SERVER_NAME:-localhost}
ADMIN_PASS=${ADMIN_PASS:-root}
SERVER_PORT=${SERVER_PORT:-6363}
WORKERS=${WORKERS:-8}
PUBLIC_URL=${PUBLIC_URL:-false}
AUTOLOGIN=${AUTOLOGIN:-false} # Either true or false
if [ ! -f /app/terminusdb/storage/prefix.db ]; then
    /app/terminusdb/utils/db_util -s "$SERVER_NAME" -k "$ADMIN_PASS" --port "$SERVER_PORT" --workers "$WORKERS" --public_url "$PUBLIC_URL" --autologin=$AUTOLOGIN
else
    /app/terminusdb/utils/db_util -s "$SERVER_NAME" -k "$ADMIN_PASS" --port "$SERVER_PORT" --workers "$WORKERS" --public_url "$PUBLIC_URL" --autologin=$AUTOLOGIN --only-config
fi
nohup swipl --no-tty --quiet -f "$FILE_DIR"/start.pl $SERVER_MODE
