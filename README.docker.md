## Docker
## <https://www.youtube.com/watch?v=YnQWBLxPoO8>

Get started with SigScale's Online Charging System (OCS) running in a Docker container image:

$ docker pull sigscale/ocs
$ docker run -ti --entrypoint bash -h host1 -v db:/home/otp/db sigscale/ocs
otp@host1:~$ bin/initialize
otp@host1:~$ exit
$ docker run -ti -h host1 -v db:/home/otp/db -p 8080:8080/tcp -p 1812:1812/udp -p 1813:1813/udp -p 3868:3868/tcp sigscale/ocs

Contact <support@sigscale.com> for further assistance.

