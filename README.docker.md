# Docker

## [Video](<https://www.youtube.com/watch?v=YnQWBLxPoO8>)

## Installation
	$ docker pull sigscale/ocs
	$ docker run -ti --entrypoint bash -h host1 -v db:/home/otp/db sigscale/ocs
	otp@host1:~$ bin/initialize
	otp@host1:~$ exit
	$ docker run -ti -h host1 -v db:/home/otp/db -p 8080:8080/tcp -p 1812:1812/udp -p 1813:1813/udp -p 3868:3868/tcp sigscale/ocs

## Support
Contact <support@sigscale.com> for further assistance.

