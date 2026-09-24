# Don't knock yourself out! Production ready debian packages are available.

## [Video](https://youtu.be/CQg9-azYjeo)

## Install SigScale package repository configuration:

### Debian 13 (trixie)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/debian-trixie/sigscale-release_1.4.7-4+debian13_all_e343ec7ca89c2d2a33b78eab85cb58c2.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

### Debian 12 (bookworm)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/debian-bookworm/sigscale-release_1.4.7-5+debian12_all_ec0edc3a2c82a64d2b13d3b08dd2b272.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

## Install SigScale OCS:
	sudo apt install ocs
	sudo systemctl enable ocs
	sudo systemctl start ocs
	sudo systemctl status ocs

## Support
Contact <support@sigscale.com> for further assistance.

