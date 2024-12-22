# Don't knock yourself out! Production ready debian packages are available.

## [Video](https://youtu.be/oL6zyGoxV70)

## Install SigScale package repository configuration:

### Ubuntu 24.04 LTS (noble)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/ubuntu-noble/sigscale-release_1.4.5-1+ubuntu24.04_all_dccb359ae044de02cad8f728fb007658.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

### Ubuntu 22.04 LTS (jammy)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/ubuntu-jammy/sigscale-release_1.4.5-1+ubuntu22.04_all_46d1cecfa87e978a64becc2cb0081fc3.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

### Ubuntu 20.04 LTS (focal)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/ubuntu-focal/sigscale-release_1.4.5-1+ubuntu20.04_all_047230675e88d1274fe97092eed30b87.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

## Install SigScale OCS:
	sudo apt install ocs
	sudo systemctl enable ocs
	sudo systemctl start ocs
	sudo systemctl status ocs

## Support
Contact <support@sigscale.com> for further assistance.

