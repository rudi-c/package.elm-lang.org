#!/bin/bash
activator dist
if [ $? -eq 0 ]; then
	scp target/universal/apiserver-1.0-SNAPSHOT.zip root@rudichen.me:~
fi
echo "Note : The server does not currently automatically run the newly uploaded file."

