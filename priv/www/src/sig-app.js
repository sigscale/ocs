/**
 * @license
 * Copyright (c) 2020 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import { setPassiveTouchGestures, setRootPath } from '@polymer/polymer/lib/utils/settings.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-drawer/app-drawer.js';
import '@polymer/app-layout/app-drawer-layout/app-drawer-layout.js';
import '@polymer/app-layout/app-header/app-header.js';
import '@polymer/app-layout/app-header-layout/app-header-layout.js';
import '@polymer/app-layout/app-scroll-effects/app-scroll-effects.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-styles/typography.js';
import '@polymer/app-route/app-location.js';
import '@polymer/app-route/app-route.js';
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/iron-selector/iron-selector.js';
import '@polymer/iron-collapse/iron-collapse.js';
import '@polymer/paper-icon-button/paper-icon-button.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-toast/paper-toast.js';
import './ocs-icons.js';
import './style-element.js';

// Gesture events like tap and track generated from touch will not be
// preventable, allowing for better scrolling performance.
setPassiveTouchGestures(true);

// Set Polymer's root path to the same value we passed to our service worker
// in `index.html`.
setRootPath(MyAppGlobals.rootPath);

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
				<app-header-layout
						has-scrolling-region>
					<app-header
							slot="header"
							condenses
							reveals
							effects="waterfall">
						<app-toolbar
								class="toolbar-top">
							<paper-icon-button
									icon="my-icons:menu"
									drawer-toggle>
							</paper-icon-button>
							<div main-title>[[viewTitle]]</div>
							<paper-icon-button
								id="refresh"
								icon="my-icons:refresh"
								on-click="refresh">
							</paper-icon-button>
							<paper-icon-button
								toggles
								icon="my-icons:overFlowMenu"
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
								tables="{{tables}}"
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
						<sig-prefix-list
								id="prefixList"
								table="{{table}}"
								loading="{{prefixLoading}}"
								name="prefixView"
								active-item="{{activePrefixItem}}">
						</sig-prefix-list>
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
						<a href="" on-click="_collapseCatalog">
							<paper-icon-button
									icon="my-icons:store">
							</paper-icon-button>
							Catalog
						</a>
						<iron-collapse id="catalog">
							<a name="offerView" href="[[rootPath]]offerView">
								<paper-icon-button
										icon="my-icons:offer">
								</paper-icon-button>
								Offerings
							</a>
							<a name="prefixView" href="[[rootPath]]prefixView">
								<paper-icon-button
										on-click="disTableList"
										class="menu-trigger menuitem"
										icon="my-icons:table">
								</paper-icon-button>
								Tables
							</a>
						</iron-collapse>
							<a href="" on-click="_collapseSubscriber">
								<paper-icon-button
										icon="my-icons:card-membership">
								</paper-icon-button>
								Subscribers 
							</a>
						<iron-collapse id="subscribers">
							<a name="serviceView" href="[[rootPath]]serviceView">
								<paper-icon-button
										icon="my-icons:devices">
								</paper-icon-button>
								Services
							</a>
							<a name="productView" href="[[rootPath]]productView">
								<paper-icon-button
										icon="my-icons:offer">
								</paper-icon-button>
								Products
							</a>
							<a name="bucketView" href="[[rootPath]]bucketView">
								<paper-icon-button
										icon="my-icons:icons:account-balance">
								</paper-icon-button>
								Balance Buckets
							</a>
						</iron-collapse>
						<a name="clientView" href="[[rootPath]]clientView">
							<paper-icon-button
								icon="my-icons:router">
							</paper-icon-button>
							Clients
						</a>
						<a name="userView" href="[[rootPath]]userView">
							<paper-icon-button
									icon="my-icons:perm-identity">
							</paper-icon-button>
							Users
						</a>
						<a href="" on-click="_collapseLogs">
							<paper-icon-button
									icon="my-icons:history">
							</paper-icon-button>
							Logs
						</a>
						<iron-collapse id="logs">
							<a name="accessView" href="[[rootPath]]accessView">
								<paper-icon-button
										icon="my-icons:data-usage">
								</paper-icon-button>
								Access
							</a>
							<a name="accountingView" href="[[rootPath]]accountingView">
								<paper-icon-button
										icon="my-icons:data-usage">
								</paper-icon-button>
								Accounting
							</a>
							<a name="balanceView" href="[[rootPath]]balanceView">
								<paper-icon-button
										icon="my-icons:data-usage">
								</paper-icon-button>
								Balance
							</a>
							<a href="" on-click="_collapseIpdr">
								<paper-icon-button
									icon="my-icons:data-usage">
								</paper-icon-button>
								IPDR
							</a>
							<iron-collapse id="ipdr">
								<a name="ipdrWlanView" href="[[rootPath]]ipdrWlanView">
									<paper-icon-button
											icon="my-icons:data-usage">
									</paper-icon-button>
									WLAN
								</a>
								<a name="ipdrVoipView" href="[[rootPath]]ipdrVoipView">
									<paper-icon-button
											icon="my-icons:data-usage">
									</paper-icon-button>
									VoIP
								</a>
							</iron-collapse>
							<a name="httpView" href="[[rootPath]]httpView">
								<paper-icon-button
									icon="my-icons:data-usage">
								</paper-icon-button>
								HTTP
							</a>
						</iron-collapse>
						<paper-dialog class="dialog" id="tableList">
							<app-toolbar>
								List of Tables
							</app-toolbar>
							<div role="listbox">
								<template id="tableList1" is="dom-repeat" items="[[tables]]">
									<paper-icon-item
											id="pagePrefix"
											class="menuitem"
											on-focused-changed="tableSelection">
										<iron-icon icon ="icons:view-list" item-icon></iron-icon>
											{{item.id}}
									</paper-icon-item>
								</template>
							</div>
							<div class="buttons">
								<paper-button
										raised
										id="tabOkButton"
										disabled
										onclick="drawer.toggle()"
										on-click="tableOk"
										class="ok-button">
									Ok
								</paper-button>
								<paper-button
										dialog-dismiss
										class="cancel-button">
									Cancel
								</paper-button>
								<paper-button
										raised
										on-tap="tableAdd"
										class="add-button">
									Add
								</paper-button>
								<paper-button
										raised
										on-tap="tableDelete"
										class="delete-buttons">
									Delete
								</paper-button>
							</div>
						</paper-dialog>
					</iron-selector>
				</app-drawer>
			</app-drawer-layout>
			<!--Modal Definitions-->
			<sig-help></sig-help>
			<sig-offer-add id="addProduct"></sig-offer-add>
			<sig-sub-add id="subscriberAdd" offers="[[offers]]"></sig-sub-add>
			<sig-sub-update active-item="[[activeServiceItem]]"></sig-sub-update>
			<sig-client-add></sig-client-add>
			<sig-client-update id="clientUpdate" active-item="[[activeClientItem]]"></sig-client-update>
			<sig-user-add></sig-user-add>
			<sig-user-update id="userUpdate" active-item="[[activeUserItem]]"></sig-user-update>
			<sig-offer-update id="updateProduct" active-item="[[activeOfferItem]]"></sig-offer-update>
			<sig-ipdr-log-files-wlan></sig-ipdr-log-files-wlan>
			<sig-ipdr-log-files-voip></sig-ipdr-log-files-voip>
			<sig-prefix-table-add></sig-prefix-table-add>
			<sig-prefix-add></sig-prefix-add>
			<sig-prefix-update></sig-prefix-update>
			<sig-bucket-add></sig-bucket-add>
			<sig-product-add active-item="[[activeProductItem]]" offers="[[offers]]"></sig-product-add>
			<iron-ajax id="deleteTableAjax"
				on-response="_deleteTableResponse"
				on-error="_deleteTableError">
			</iron-ajax>
		`;
	}


	_collapseCatalog(event) {
		var cat = document.body.querySelector('sig-app').shadowRoot.getElementById('catalog');
		if(cat.opened == false) {
			cat.show();
		} else {
			cat.hide();
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

	tableOk() {
		document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').shadowRoot.getElementById('prefixGrid');
	}

	disTableList() {
		this.$.tableList.open();
		document.body.querySelector('sig-app').shadowRoot.getElementById('offerList').shadowRoot.getElementById('offerGrid');
	}

	tableAdd() {
		document.body.querySelector('sig-app').shadowRoot.getElementById('sig-prefix-table-add').shadowRoot.getElementById('addPrefixTableModal').open();
		this.$.tableList.close();
	}

	tableSelection(e) {
		if(e.model.item && e.model.item.id) {
			var prefixTab = document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').table;
			prefixTab  = e.model.item.id;
			document.body.querySelector('sig-app').shadowRoot.getElementById('tabOkButton').disabled = false;
		} else {
			document.body.querySelector('sig-app').shadowRoot.getElementById('tabOkButton').disabled = true;
		}
	}

	tableDelete(event) {
		this.$.deleteTableAjax.method = "DELETE";
		this.$.deleteTableAjax.url = "/catalogManagement/v2/pla/" + this.$.prefixList.table;
		this.$.deleteTableAjax.generateRequest();
	}

	_deleteTableResponse(event) {
		this.$.tableList.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('offerList').shadowRoot.getElementById('getTableAjax').generateRequest();
	}

	_overFlowMenu() {
//		import('./sig-help.js');
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
			prefixLoading: {
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
			}
		};
	}

	static get observers() {
		return [
			'_routePageChanged(routeData.page)',
			'_loadingChanged(offerLoading, serviceLoading, clientLoading, userLoading, accessLoading, accountingLoading, ipdrWlanLoading, ipdrVoipLoading, httpLoading, prefixLoading, balanceLoading, productLoading, bucketLoading)'
		];
	}

	_routePageChanged(page) {
// Show the corresponding page according to the route.
//
// If no page was found in the route data, page will be an empty string.
// Show 'inventoryView' in that case. And if the page doesn't exist, show 'view404'.
		if (['offerView', 'serviceView', 'clientView', 'userView', 'accessView', 'accountingView', 'ipdrWlanView', 'ipdrVoipView', 'httpView', 'prefixView', 'balanceView', 'productView', 'bucketView'].indexOf(page) !== -1) {
			this.page = page;
		}
		switch (this.page) {
			case 'offerView':
				this.viewTitle = 'Product Offering';
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
			case 'ipdrWlanView':
				this.viewTitle = 'IPDR WLAN';
				break;
			case 'ipdrVoipView':
				this.viewTitle = 'IPDR Voice';
				break;
			case 'httpView':
				this.viewTitle = 'HTTP Log';
				break;
			case 'prefixView':
				this.viewTitle = 'Tariff Table';
				break;
			case 'balanceView':
				this.viewTitle = 'Accumulated Balance';
				break;
			case 'productView':
				this.viewTitle = 'Product Subscriptions';
				break;
			case 'bucketView':
				this.viewTitle = 'Balance Buckets';
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
//				import('./sig-access-list.js');
				break;
			case 'accountingView':
//				import('./sig-accounting-list.js');
				break;
			case 'ipdrWlanView':
//				import('./sig-ipdr-list-wlan.js');
//				import('./sig-ipdr-log-files-wlan.js');
				break;
			case 'ipdrVoipView':
//				import('./sig-ipdr-list-voip.js');
//				import('./sig-ipdr-log-files-voip.js');
				break;
			case 'httpView':
//				import('./sig-http-list.js');
				break;
			case 'prefixView':
				import('./sig-prefix-list.js');
				import('./sig-prefix-add.js');
//				import('./sig-prefix-table-add.js');
//				import('./sig-prefix-update.js');
				break;
			case 'balanceView':
//				import('./sig-balance-list.js');
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
		if(this.offerLoading || this.serviceLoading || this.clientLoading || this.userLoading || this.accessLoading || this.accountingLoading || this.ipdrWlanLoading || this.ipdrVoipLoading || this.httpLoading || this.prefixLoading || this.balanceLoading || this.productLoading || this.bucketLoading) {
			this.loading = true;
		} else {
			this.loading = false;
		}
	}
}

window.customElements.define('sig-app', SigApp);
