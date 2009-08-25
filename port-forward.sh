set -x
LOCAL_PORT=$1
REMOTE_PORT=$2
HOST=$3

function usage () {
    echo "Please specify a port to forward: $0 local-port remote-port [host=localhost]"
}

if [ -z "$LOCAL_PORT" ]; then
    usage
    exit 1
fi

if [ -z "$REMOTE_PORT" ]; then
    REMOTE_PORT=$LOCAL_PORT
fi

if [ -z "$HOST" ]; then
    HOST=localhost
fi

ssh -L$LOCAL_PORT:localhost:$REMOTE_PORT $HOST
