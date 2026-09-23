# Don't knock yourself out! Production ready debian packages are available.

## [Video](https://youtu.be/CQg9-azYjeo)

## Install SigScale package repository configuration:

### Debian 13 (trixie)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/debian-trixie/sigscale-release_1.4.7-3+debian13_all_0b5c0d828317aedd8f2e02b85b4b0c87.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

### Debian 12 (bookworm)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/debian-bookworm/sigscale-release_1.4.7-4+debian12_all_8e03bf8aef443541b5a24034767e9cae.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

## Install SigScale OCS:
	sudo apt install ocs
	sudo systemctl enable ocs
	sudo systemctl start ocs
	sudo systemctl status ocs

## Support
Contact <support@sigscale.com> for further assistance.

