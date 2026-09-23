# Don't knock yourself out! Production ready debian packages are available.

## [Video](https://youtu.be/oL6zyGoxV70)

## Install SigScale package repository configuration:

### Ubuntu 26.04 LTS (resolute)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/ubuntu-resolute/sigscale-release_1.4.7-4+ubuntu26.04_all_1b1cafb923c5fe94cf6f0acfc5ad8db8.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

### Ubuntu 24.04 LTS (noble)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/ubuntu-noble/sigscale-release_1.4.7-1+ubuntu24.04_all_28d2a9f8f10e9abdd02f6781ce1b3f22.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

## Install SigScale OCS:
	sudo apt install ocs
	sudo systemctl enable ocs
	sudo systemctl start ocs
	sudo systemctl status ocs

## Support
Contact <support@sigscale.com> for further assistance.

