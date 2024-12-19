/**
 * Copyright 2016 - 2024 SigScale Global Inc.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *      http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import { setPassiveTouchGestures, setRootPath } from '@polymer/polymer/lib/utils/settings.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-drawer/app-drawer.js';
import '@polymer/app-layout/app-drawer-layout/app-drawer-layout.js';
import '@polymer/app-layout/app-header/app-header.js';
import '@polymer/app-layout/app-header-layout/app-header-layout.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/app-route/app-location.js';
import '@polymer/app-route/app-route.js';
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/iron-selector/iron-selector.js';
import '@polymer/iron-collapse/iron-collapse.js';
import '@polymer/paper-icon-button/paper-icon-button.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-toast/paper-toast.js';
import '@polymer/paper-item/paper-item.js';
import './ocs-icons.js';
import './style-element.js';

// Gesture events like tap and track generated from touch will not be
// preventable, allowing for better scrolling performance.
setPassiveTouchGestures(true);

// Set Polymer's root path to the same value we passed to our service worker
// in `index.html`.
setRootPath(SigScaleGlobals.rootPath);

class SigApp extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<app-location
					route="{{route}}"
					url-space-regex="^[[rootPath]]">
			</app-location>
			<app-route
					route="{{route}}"
					pattern="[[rootPath]]:page"
					data="{{routeData}}"
					tail="{{subroute}}">
			</app-route>
			<app-drawer-layout
					force-narrow
					fullbleed>
				<app-header-layout>
					<app-header
							slot="header">
						<app-toolbar
								class="toolbar-top">
							<paper-icon-button
									icon="ocs-icons:menu"
									drawer-toggle>
							</paper-icon-button>
							<div main-title>[[viewTitle]]</div>
							<paper-icon-button
								id="refresh"
								icon="ocs-icons:refresh"
								on-click="refresh">
							</paper-icon-button>
							<paper-icon-button
								toggles
								icon="ocs-icons:overFlowMenu"
								id="overFlowIcon"
								active="{{overFlowActive}}"
								on-click="_overFlowMenu">
							</paper-icon-button>
						</app-toolbar>
						<paper-progress
							indeterminate
							class="slow red"
							disabled="{{!loading}}">
						</paper-progress>
					</app-header>
					<iron-pages
							id="load"
							role="main"
							selected="[[page]]"
							attr-for-selected="name">
						<sig-offer-list
								id="offerList"
								loading="{{offerLoading}}"
								offers="{{offers}}"
								name="offerView"
								active-item="{{activeOfferItem}}">
						</sig-offer-list>
						<sig-sub-list
								id="serviceList"
								loading="{{serviceLoading}}"
								name="serviceView"
								active-item="{{activeServiceItem}}">
						</sig-sub-list>
						<sig-client-list
								id="clientList"
								loading="{{clientLoading}}"
								name="clientView"
								active-item="{{activeClientItem}}">
						</sig-client-list>
						<sig-user-list
								id="userList"
								loading="{{userLoading}}"
								name="userView"
								active-item="{{activeUserItem}}">
						</sig-user-list>
						<sig-access-list
								id="accessList"
								loading="{{accessLoading}}"
								name="accessView"
								active-item="{{activeAccessItem}}">
						</sig-access-list>
						<sig-accounting-list
								id="accountingList"
								loading="{{accountingLoading}}"
								name="accountingView"
								active-item="{{activeAccountingItem}}">
						</sig-accounting-list>
						<sig-ipdr-list-wlan
								id="ipdrLogListWlan"
								loading="{{ipdrWlanLoading}}"
								name="ipdrWlanView"
								active-item="{{ipdrWlanItem}}">
						</sig-ipdr-list-wlan>
						<sig-ipdr-list-voip
								id="ipdrLogListVoip"
								loading="{{ipdrVoipLoading}}"
								name="ipdrVoipView"
								active-item="{{ipdrWlanItem}}">
						</sig-ipdr-list-voip>
						<sig-http-list
								id="httpList"
								loading="{{httpLoading}}"
								name="httpView">
						</sig-http-list>
						<sig-tariff-rate-list
								id="rateList"
								active-item="{{activePrefixItem}}"
								active-table-name="{{rateTable}}"
								loading="{{rateLoading}}"
								name="rateView">
						</sig-tariff-rate-list>
						<sig-tariff-period-list
								id="periodList"
								active-item="{{activePeriodItem}}"
								active-table-name="{{periodTable}}"
								loading="{{periodLoading}}"
								name="periodView">
						</sig-tariff-period-list>
						<sig-tariff-roaming-list
								id="roamingList"
								active-item="{{activeRoamingItem}}"
								active-table-name="{{roamingTable}}"
								loading="{{roamingLoading}}"
								name="roamingView">
						</sig-tariff-roaming-list>
						<sig-balance-list
								id="balanceList"
								loading="{{balanceLoading}}"
								name="balanceView"
								active-item="{{activeBalanceItem}}">
						</sig-balance-list>
						<sig-product-list
								id="productList"
								loading="{{productLoading}}"
								name="productView"
								active-item="{{activeProductItem}}">
						</sig-product-list>
						<sig-bucket-list
								id="bucketList"
								loading="{{bucketLoading}}"
								name="bucketView"
								active-item="{{activeBucketItem}}">
						</sig-bucket-list>
						<sig-policy-list
								id="policyList"
								active-table-name="{{policyTable}}"
								loading="{{policyLoading}}"
								name="policyView">
						</sig-policy-list>
						<sig-dashboard
								id="dashBoard"
								loading="{{dashLoading}}"
								name="dashView">
						</sig-dashboard>
					</iron-pages>
					<paper-toast
							id="restError"
							class="fit-bottom"
							duration="8000">
					</paper-toast>
				</app-header-layout>
				<app-drawer
						id="drawer"
						slot="drawer">
					<iron-selector
							selected="[[page]]"
							attr-for-selected="name"
							class="drawer-list"
							role="navigation">
						<a name="dashView" href="[[rootPath]]dashView">
							<paper-icon-button
								icon="ocs-icons:dashboard">
							</paper-icon-button>
							Dashboard
						</a>
						<a href="" on-click="_collapseCatalog">
							<paper-icon-button
									icon="ocs-icons:store">
							</paper-icon-button>
							Catalog
						</a>
						<iron-collapse id="catalog">
							<a name="offerView" href="[[rootPath]]offerView">
								<paper-icon-button
										icon="ocs-icons:offer">
								</paper-icon-button>
								Offerings
							</a>
							<a href="" on-click="_collapseTariff">
								<paper-icon-button
										icon="ocs-icons:table">
								</paper-icon-button>
								Tariff
							</a>
							<iron-collapse id="tariff">
								<a name="rateView" href="[[rootPath]]rateView">
									<paper-icon-button
											icon="ocs-icons:table">
									</paper-icon-button>
									Rate
								</a>
								<a name="periodView" href="[[rootPath]]periodView">
									<paper-icon-button
											icon="ocs-icons:table">
									</paper-icon-button>
									Period
								</a>
								<a name="roamingView" href="[[rootPath]]roamingView">
									<paper-icon-button
											icon="ocs-icons:table">
									</paper-icon-button>
									Roaming
								</a>
							</iron-collapse>
							<a name="policyView" href="[[rootPath]]policyView">
								<paper-icon-button
										icon="icons:verified-user">
								</paper-icon-button>
								Policy
							</a>
						</iron-collapse>
							<a href="" on-click="_collapseSubscriber">
								<paper-icon-button
										icon="ocs-icons:card-membership">
								</paper-icon-button>
								Subscribers 
							</a>
						<iron-collapse id="subscribers">
							<a name="serviceView" href="[[rootPath]]serviceView">
								<paper-icon-button
										icon="ocs-icons:devices">
								</paper-icon-button>
								Services
							</a>
							<a name="productView" href="[[rootPath]]productView">
								<paper-icon-button
										icon="ocs-icons:offer">
								</paper-icon-button>
								Products
							</a>
							<a name="bucketView" href="[[rootPath]]bucketView">
								<paper-icon-button
										icon="ocs-icons:icons:account-balance">
								</paper-icon-button>
								Balance Buckets
							</a>
						</iron-collapse>
						<a name="clientView" href="[[rootPath]]clientView">
							<paper-icon-button
								icon="ocs-icons:router">
							</paper-icon-button>
							Clients
						</a>
						<a name="userView" href="[[rootPath]]userView">
							<paper-icon-button
									icon="ocs-icons:perm-identity">
							</paper-icon-button>
							Users
						</a>
						<a href="" on-click="_collapseLogs">
							<paper-icon-button
									icon="ocs-icons:history">
							</paper-icon-button>
							Logs
						</a>
						<iron-collapse id="logs">
							<a name="accessView" href="[[rootPath]]accessView">
								<paper-icon-button
										icon="ocs-icons:data-usage">
								</paper-icon-button>
								Access
							</a>
							<a name="accountingView" href="[[rootPath]]accountingView">
								<paper-icon-button
										icon="ocs-icons:data-usage">
								</paper-icon-button>
								Accounting
							</a>
							<a name="balanceView" href="[[rootPath]]balanceView">
								<paper-icon-button
										icon="ocs-icons:data-usage">
								</paper-icon-button>
								Balance
							</a>
							<a href="" on-click="_collapseIpdr">
								<paper-icon-button
									icon="ocs-icons:data-usage">
								</paper-icon-button>
								IPDR
							</a>
							<iron-collapse id="ipdr">
								<a name="ipdrWlanView" href="[[rootPath]]ipdrWlanView">
									<paper-icon-button
											icon="ocs-icons:data-usage">
									</paper-icon-button>
									WLAN
								</a>
								<a name="ipdrVoipView" href="[[rootPath]]ipdrVoipView">
									<paper-icon-button
											icon="ocs-icons:data-usage">
									</paper-icon-button>
									VoIP
								</a>
							</iron-collapse>
							<a name="httpView" href="[[rootPath]]httpView">
								<paper-icon-button
									icon="ocs-icons:data-usage">
								</paper-icon-button>
								HTTP
							</a>
						</iron-collapse>
					</iron-selector>
				</app-drawer>
			</app-drawer-layout>
			<!--Modal Definitions-->
			<sig-help id="ocsGetHelp" active="[[overFlowActive]]"></sig-help>
			<sig-offer-add id="addOffer" offers="[[offers]]"></sig-offer-add>
			<sig-sub-add id="subscriberAdd" offers="[[offers]]"></sig-sub-add>
			<sig-sub-update active-item="[[activeServiceItem]]"></sig-sub-update>
			<sig-client-add></sig-client-add>
			<sig-client-update id="clientUpdate" active-item="[[activeClientItem]]"></sig-client-update>
			<sig-user-add></sig-user-add>
			<sig-user-update id="userUpdate" active-item="[[activeUserItem]]"></sig-user-update>
			<sig-offer-update id="updateOffer" offers="[[offers]]" active-item="[[activeOfferItem]]"></sig-offer-update>
			<sig-ipdr-log-files-wlan></sig-ipdr-log-files-wlan>
			<sig-ipdr-log-files-voip id="ipdrLogs"></sig-ipdr-log-files-voip>
			<sig-rate-table-list></sig-rate-table-list>
			<sig-period-table-list></sig-period-table-list>
			<sig-roaming-table-list></sig-roaming-table-list>
			<sig-rate-table-add></sig-rate-table-add>
			<sig-period-table-add></sig-period-table-add>
			<sig-roaming-table-add></sig-roaming-table-add>
			<sig-tariff-rate-add></sig-tariff-rate-add>
			<sig-tariff-period-add></sig-tariff-period-add>
			<sig-tariff-roaming-add></sig-tariff-roaming-add>
			<sig-tariff-rate-update active-item="[[activePrefixItem]]" active-table-name="{{rateTable}}"></sig-tariff-rate-update>
			<sig-tariff-period-update active-item="[[activePeriodItem]]"></sig-tariff-period-update>
			<sig-tariff-roaming-update active-item="[[activeRoamingItem]]"></sig-tariff-roaming-update>
			<sig-bucket-add></sig-bucket-add>
			<sig-product-add active-item="[[activeProductItem]]" offers="[[offers]]"></sig-product-add>
			<sig-policy-add id="policyAdd"></sig-policy-add>
			<sig-policy-table-add></sig-policy-table-add>
		`;
	}


	refresh() {
		if(this.$.load.selected == "dashView") {
			var dash = document.body.querySelector('sig-app').shadowRoot.getElementById('dashBoard');
			if (!dash.loading) {
				dash._healthChart();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "offerView") {
			var offer = document.body.querySelector('sig-app').shadowRoot.getElementById('offerList');
			if (!offer.loading) {
				offer.splice('offers', 0, offer.offers.length);
				offer.shadowRoot.getElementById('offerGrid').scrollToIndex();
				offer.shadowRoot.getElementById('offerGrid').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "serviceView") {
			var service = document.body.querySelector('sig-app').shadowRoot.getElementById('serviceList');
			if (!service.loading) {
				service.shadowRoot.getElementById('subscriberGrid').scrollToIndex();
				service.shadowRoot.getElementById('subscriberGrid').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "clientView") {
			var client = document.body.querySelector('sig-app').shadowRoot.getElementById('clientList');
			if (!client.loading) {
				client.shadowRoot.getElementById('clientGrid').scrollToIndex();
				client.shadowRoot.getElementById('clientGrid').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "userView") {
			var user = document.body.querySelector('sig-app').shadowRoot.getElementById('userList');
			if (!user.loading) {
				user.shadowRoot.getElementById('userGrid').scrollToIndex();
				user.shadowRoot.getElementById('userGrid').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "accessView") {
			var access = document.body.querySelector('sig-app').shadowRoot.getElementById('accessList')
			if (!access.loading) {
				access.shadowRoot.getElementById('accessGrid').scrollToIndex();
				access.shadowRoot.getElementById('accessGrid').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "accountingView") {
			var account = document.body.querySelector('sig-app').shadowRoot.getElementById('accountingList');
			if (!account.loading) {
				account.shadowRoot.getElementById('accountingGrid').scrollToIndex();
				account.shadowRoot.getElementById('accountingGrid').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "ipdrWlanView") {
			var wlan = document.body.querySelector('sig-app').shadowRoot.getElementById('ipdrLogListWlan');
			if (!wlan.loading) {
				wlan.shadowRoot.getElementById('ipdrGrid').scrollToIndex();
				wlan.shadowRoot.getElementById('ipdrGrid').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "ipdrVoipView") {
			var voip = document.body.querySelector('sig-app').shadowRoot.getElementById('ipdrLogListVoip');
			if (!voip.loading) {
				voip.shadowRoot.getElementById('ipdrGridVoip').scrollToIndex();
				voip.shadowRoot.getElementById('ipdrGridVoip').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "httpView") {
			var http = document.body.querySelector('sig-app').shadowRoot.getElementById('httpList');
			if (!http.loading) {
				http.shadowRoot.getElementById('getHttp').generateRequest();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "rateView") {
			var rateList = document.body.querySelector('sig-app').shadowRoot.getElementById('rateList');
			if (!rateList.loading) {
				rateList.shadowRoot.getElementById('getPrefixRows').generateRequest();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "periodView") {
			var periodList = document.body.querySelector('sig-app').shadowRoot.getElementById('periodList');
			if (!periodList.loading) {
				periodList.shadowRoot.getElementById('getPeriodRows').generateRequest();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "roamingView") {
			var roamList = document.body.querySelector('sig-app').shadowRoot.getElementById('roamList');
			if (!roamList.loading) {
				roamList.shadowRoot.getElementById('getRoamingRows').generateRequest();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "policyView") {
		var policy = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list');
			if (!policy.loading) {
				policy.shadowRoot.getElementById('tableList').open();
				policy.shadowRoot.getElementById('policyGrid').scrollToIndex();
				policy.shadowRoot.getElementById('policyGrid').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "balanceView") {
			var balance = document.body.querySelector('sig-app').shadowRoot.getElementById('balanceList');
			if (!balance.loading) {
				balance.shadowRoot.getElementById('balanceGrid').scrollToIndex();
				balance.shadowRoot.getElementById('balanceGrid').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "productView") {
			var product = document.body.querySelector('sig-app').shadowRoot.getElementById('productList');
			if (!product.loading) {
				product.shadowRoot.getElementById('productInventoryGrid').scrollToIndex();
				product.shadowRoot.getElementById('productInventoryGrid').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
		if(this.$.load.selected == "bucketView") {
			var bucket = document.body.querySelector('sig-app').shadowRoot.getElementById('bucketList');
			if (!bucket.loading) {
				bucket.shadowRoot.getElementById('balanceBucketGrid').scrollToIndex();
				bucket.shadowRoot.getElementById('balanceBucketGrid').clearCache();
			} else {
				console.log('Have patience dude!');
			}
		}
	}

	_collapseCatalog(event) {
		var cat = document.body.querySelector('sig-app').shadowRoot.getElementById('catalog');
		if(cat.opened == false) {
			cat.show();
		} else {
			cat.hide();
		}
	}

	_collapseTariff(event) {
		var tar = document.body.querySelector('sig-app').shadowRoot.getElementById('tariff');
		if(tar.opened == false) {
			import('./sig-period-table-list.js');
			import('./sig-rate-table-list.js');
			import('./sig-roaming-table-list.js');
			tar.show();
		} else {
			tar.hide();
		}
	}

	_collapseSubscriber(event) {
		var cat = document.body.querySelector('sig-app').shadowRoot.getElementById('subscribers');
		if(cat.opened == false) {
			cat.show();
		} else {
			cat.hide();
		}
	}

	_collapseLogs(event) {
		var cat = document.body.querySelector('sig-app').shadowRoot.getElementById('logs');
		if(cat.opened == false) {
			cat.show();
		} else {
			cat.hide();
		}
	}

	_collapseIpdr(event) {
		var cat = document.body.querySelector('sig-app').shadowRoot.getElementById('ipdr');
		if(cat.opened == false) {
			cat.show();
		} else {
			cat.hide();
		}
	}

	_overFlowMenu() {
		import('./sig-help.js');
	}

	 static get properties() {
		return {
			page: {
				type: String,
				reflectToAttribute: true,
				observer: '_pageChanged'
			},
			routeData: Object,
			ubroute: Object,
			viewTitle: {
				type: String
			},
			loading: {
				type: Boolean,
				value: false
			},
			offerLoading: {
				type: Boolean
			},
			serviceLoading: {
				type: Boolean
			},
			clientLoading: {
				type: Boolean
			},
			userLoading: {
				type: Boolean
			},
			accessLoading: {
				type: Boolean
			},
			accountingLoading: {
				type: Boolean
			},
			ipdrWlanLoading: {
				type: Boolean
			},
			ipdrVoipLoading: {
				type: Boolean
			},
			httpLoading: {
				type: Boolean
			},
			rateLoading: {
				type: Boolean
			},
			periodLoading: {
				type: Boolean
			},
			roamingLoading: {
				type: Boolean
			},
			periodLoading: {
				type: Boolean
			},
			balanceLoading: {
				type: Boolean
			},
			productLoading: {
				type: Boolean
			},
			bucketLoading: {
				type: Boolean
			},
			dashLoading: {
				type: Boolean
			},
			rateTable: {
				type: String
			},
			periodTable: {
				type: String
			},
			roamingTable: {
				type: String
			},
			policyTable: {
				type: String
			}
		};
	}

	static get observers() {
		return [
			'_routePageChanged(routeData.page)',
			'_loadingChanged(dashLoading, offerLoading, serviceLoading, clientLoading, userLoading, accessLoading, accountingLoading, ipdrWlanLoading, ipdrVoipLoading, httpLoading, rateLoading, periodLoading, roamingLoading, balanceLoading, productLoading, bucketLoading)'
		];
	}

	_routePageChanged(page) {
// Show the corresponding page according to the route.
//
// If no page was found in the route data, page will be an empty string.
// Show 'dashView' in that case. And if the page doesn't exist, show 'view404'.
		if(this.page == undefined){
			this.page = 'dashView';
		}
		if (['dashView', 'offerView', 'serviceView', 'clientView', 'userView', 'accessView', 'accountingView', 'ipdrWlanView', 'ipdrVoipView', 'httpView', 'rateView', 'periodView', 'roamingView','policyView', 'balanceView', 'productView', 'bucketView'].indexOf(page) !== -1) {
			this.page = page;
		}
		switch (this.page) {
			case 'dashView':
				this.viewTitle = 'Dashboard';
				break;
			case 'offerView':
				this.viewTitle = 'Product Offerings';
				break;
			case 'serviceView':
				this.viewTitle = 'Services';
				break;
			case 'clientView':
				this.viewTitle = 'Clients';
				break;
			case 'userView':
				this.viewTitle = 'Users';
				break;
			case 'accessView':
				this.viewTitle = 'Authentication Log';
				break;
			case 'accountingView':
				this.viewTitle = 'Accounting Log';
				break;
			case 'balanceView':
				this.viewTitle = 'Balance Log';
				break;
			case 'ipdrWlanView':
				this.viewTitle = 'IPDR WLAN Log';
				break;
			case 'ipdrVoipView':
				this.viewTitle = 'IPDR Voice Log';
				break;
			case 'httpView':
				this.viewTitle = 'HTTP Log';
				break;
			case 'rateView':
				if(this.rateTable) {
					this.viewTitle = 'Tariff: ' + this.rateTable;
				} else {
					this.viewTitle = 'Tariff Tables';
				}
				var rateTableList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-rate-table-list');
				rateTableList.shadowRoot.getElementById('tariffRateList').open();
				rateTableList.shadowRoot.getElementById('getRateTables').generateRequest();
				break;
			case 'periodView':
				if(this.periodTable) {
					this.viewTitle = 'Tariff: ' + this.periodTable;
				} else {
					this.viewTitle = 'Tariff Tables';
				}
				var periodTableList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-period-table-list');
				periodTableList.shadowRoot.getElementById('tariffPeriodList').open();
				periodTableList.shadowRoot.getElementById('getPeriodTables').generateRequest();
				break;
			case 'roamingView':
				if(this.roamingTable) {
					this.viewTitle = 'Tariff: ' + this.roamingTable;
				} else {
					this.viewTitle = 'Tariff Tables';
				}
				var roamTableList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-roaming-table-list');
				roamTableList.shadowRoot.getElementById('tariffRoamingList').open();
				roamTableList.shadowRoot.getElementById('getRoamingTables').generateRequest();
				break;
			case 'productView':
				this.viewTitle = 'Product Subscriptions';
				break;
			case 'bucketView':
				this.viewTitle = 'Balance Buckets';
				break;
			case 'policyView':
				if(this.policyTable) {
					this.viewTitle = 'Policy: ' + this.policyTable;
				} else {
					this.viewTitle = 'Policy Tables';
				}
				break;
			default:
				this.viewTitle = 'Online Charging System';
		}
// Close a non-persistent drawer when the page & route are changed.
		if (!this.$.drawer.persistent) {
			this.$.drawer.close();
		}
	}

	_pageChanged(page) {
// Import the page component on demand.
//
// Note: `polymer build` doesn't like string concatenation in the import
// statement, so break it up.
		switch (page) {
			case 'dashView':
				import('./sig-dashboard.js');
				break;
			case 'offerView':
				import('./sig-offer-add.js');
				import('./sig-offer-update.js');
				import('./sig-offer-list.js');
				break;
			case 'serviceView': 
				import('./sig-offer-list.js');
				import('./sig-sub-add.js');
				import('./sig-sub-update.js');
				import('./sig-sub-list.js');
				break;
			case 'clientView':
				import('./sig-client-add.js');
				import('./sig-client-update.js');
				import('./sig-client-list.js');
				break;
			case 'userView':
				import('./sig-user-add.js');
				import('./sig-user-list.js');
				import('./sig-user-update.js');
				break;
			case 'accessView':
				import('./sig-access-list.js');
				break;
			case 'accountingView':
				import('./sig-accounting-list.js');
				break;
			case 'ipdrWlanView':
				import('./sig-ipdr-list-wlan.js');
//				import('./sig-ipdr-log-files-wlan.js');
				break;
			case 'ipdrVoipView':
				import('./sig-ipdr-list-voip.js');
				break;
			case 'httpView':
				import('./sig-http-list.js');
				break;
			case 'rateView':
				import('./sig-rate-table-add.js');
				import('./sig-tariff-rate-list.js');
				import('./sig-tariff-rate-add.js');
				import('./sig-tariff-rate-update.js');
				break;
			case 'periodView':
				import('./sig-period-table-add.js');
				import('./sig-tariff-period-list.js');
				import('./sig-tariff-period-add.js');
				import('./sig-tariff-period-update.js');
				break;
			case 'roamingView':
				import('./sig-roaming-table-add.js');
				import('./sig-tariff-roaming-list.js');
				import('./sig-tariff-roaming-add.js');
				import('./sig-tariff-roaming-update.js');
				break;
			case 'policyView':
				import('./sig-policy-list.js');
				import('./sig-policy-table-add.js');
				import('./sig-policy-add.js');
				break;
			case 'balanceView':
				import('./sig-balance-list.js');
				break;
			case 'productView':
				import('./sig-offer-list.js');
				import('./sig-product-add.js');
				import('./sig-product-list.js');
				break;
			case 'bucketView':
				import('./sig-bucket-add.js');
				import('./sig-bucket-list.js ');
				break;
		}
	}

	_loadingChanged() {
		if(this.dashLoading || this.offerLoading || this.serviceLoading || this.clientLoading || this.userLoading || this.accessLoading || this.accountingLoading || this.ipdrWlanLoading || this.ipdrVoipLoading || this.httpLoading || this.rateLoading || this.periodLoading || this.roamingLoading || this.balanceLoading || this.productLoading || this.bucketLoading) {
			this.loading = true;
		} else {
			this.loading = false;
		}
	}
}

window.customElements.define('sig-app', SigApp);
