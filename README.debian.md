# Don't knock yourself out! Production ready debian packages are available.

## [Video](https://youtu.be/CQg9-azYjeo)

## Install SigScale package repository configuration:

### Debian 13 (trixie)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/debian-trixie/sigscale-release_1.4.8-1+debian13_all_2da4401193b5a5378ad51d04b4c1cac0.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

### Debian 12 (bookworm)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/debian-bookworm/sigscale-release_1.4.8-1+debian12_all_65b9ded95cea6fb72261d229ffd42647.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

## Install SigScale OCS:
	sudo apt install ocs
	sudo systemctl enable ocs
	sudo systemctl start ocs
	sudo systemctl status ocs

## Support
Contact <support@sigscale.com> for further assistance.

