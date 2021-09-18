/**
 * Copyright 2016 - 2021 SigScale Global Inc.
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
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/paper-checkbox/paper-checkbox.js'
import '@polymer/paper-icon-button/paper-icon-button.js';
import '@polymer/iron-collapse/iron-collapse.js';
import '@polymer/paper-tabs/paper-tabs.js';
import '@polymer/paper-tooltip/paper-tooltip.js';
import './style-element.js';

class offerAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog
					class="dialog"
					id="addOfferModal"
					modal>
				<app-toolbar>
					<paper-tabs
							selected="{{selected}}">
						<paper-tab>
							<h2>Offering</h2>
						</paper-tab>
						<paper-tab>
							<h2>Prices</h2>
						</paper-tab>
						<paper-tab>
							<h2>Alterations</h2>
						</paper-tab>
					</paper-tabs>
				</app-toolbar>
				<paper-progress
						id="progressId"
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<iron-pages
						selected="{{selected}}">
					<div>
						<div>
							<paper-input
									label="Name"
									value="{{offerName}}">
							</paper-input>
							<paper-tooltip>
								Name of the Product Offering.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									label="Description"
									value="{{offerDescription}}">
							</paper-input>
							<paper-tooltip>
								Description of the Product Offering.
							</paper-tooltip>
						</div>
						<div>
							<span>Bundled Products</span>
							<paper-icon-button
									id="onClickBundle"
									suffix
									icon="arrow-drop-down"
									on-tap="_onClickBundle">
							</paper-icon-button>
							<paper-tooltip>
								A compound Product Offering bundles other Product Offerings together.
							</paper-tooltip>
						</div>
						<iron-collapse
								id="addBundle">
							<template
									is="dom-repeat"
									items="{{bundleOffers}}">
								<paper-checkbox
										checked="{{item.checked}}"> 
									{{item.name}}
								</paper-checkbox>
							</template>
						</iron-collapse>
						<div>
							<paper-dropdown-menu
									id="addOfferProductSpecDrop"
									on-selected-item-changed="_checkProductSpec"
									value="{{offerAddSpec}}"
									no-animations="true"
									label="Product Specification">
								<paper-listbox
										id="addOfferProductSpecList"
										slot="dropdown-content">
									<paper-item>
										Prepaid Data
									</paper-item>
									<paper-item>
										Prepaid Voice
									</paper-item>
									<paper-item>
										Prepaid SMS
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Product Specification provides characteristics and values to charge service types.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									type="datetime-local"
									label="Start Date"
									value="{{offerStartDate}}">
							</paper-input>
							<paper-tooltip>
								Start of Product Offering validity period.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									type="datetime-local"
									label="End Date"
									value="{{offerEndDate}}">
							</paper-input>
							<paper-tooltip>
								End of Product Offering validity period.
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									id="addOfferProductStatusDrop"
									value="{{offerAddStatus}}"
									no-animations="true"
									label="Status">
								<paper-listbox
										id="addOfferProductStatusList"
										slot="dropdown-content">
									<paper-item>
										In Study
									</paper-item>
									<paper-item>
										In Design
									</paper-item>
									<paper-item>
										In Test
									</paper-item>
									<paper-item>
										Active
									</paper-item>
									<paper-item>
										Rejected
									</paper-item>
									<paper-item>
										Launched
									</paper-item>
									<paper-item>
										Retired
									</paper-item>
									<paper-item>
										Obsolete
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
						</div>
						<div>
							<span>Characteristics</span>
							<paper-icon-button
									id="onClickOfferChars"
									suffix
									icon="arrow-drop-down"
									on-tap="_onClickOfferChars">
							</paper-icon-button>
							<paper-tooltip>
								Collapse or expand the list of available Product Offering characteristics.
							</paper-tooltip>
						</div>
						<iron-collapse
								id="addOfferChars">
							<div>
								<div>
									<paper-input
											id="addOfferCharReserveSession"
											allowed-pattern="[0-9mh]"
											pattern="^[0-9]+[mh]?$"
											auto-validate
											label="RADIUS Reserve Session Time"
											value="0">
									</paper-input>
									<paper-tooltip>
										Amount of time to reserve on RADIUS session authorization and session timeout for reauthorization.
									</paper-tooltip>
								</div>
								<div>
									<paper-input
										label="Redirect Server"
										pattern="(^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3})|(^http://.+)|(sip:.+)"
										auto-validate
										value="{{redirectAddress}}">
									</paper-input>
									<paper-tooltip>
										An IPv4/IPv6 address, HTTP URL or SIP URI to which traffic will be redirected when out of credit.
									</paper-tooltip>
								</div>
								<div>
									<paper-input
										label="Service Identifier"
										type="number"
										auto-validate
										value="{{serviceIdentifier}}">
									</paper-input>
									<paper-tooltip>
										characteristic serviceIdentifiers
									</paper-tooltip>
								</div>
							</div>
							<div>
								<paper-dropdown-menu
										id="policyName"
										value="{{offerPolicy}}"
										no-animations="true"
										label="Policy Table">
									<paper-listbox
											slot="dropdown-content">
										<template
												is="dom-repeat"
												items="{{policyTables}}">
											<paper-item>
												{{item.name}}
											</paper-item>
										</template>
									</paper-listbox>
								</paper-dropdown-menu>
								<paper-tooltip>
									Name of a Policy table of rules to install/activate on PCEF.
								</paper-tooltip>
							</div>
						</iron-collapse>
						<div class="buttons">
							<paper-button
									raised
									class="submit-button"
									on-tap="_addOffer">
								Submit
							</paper-button>
							<paper-button
									dialog-dismiss
									class="cancel-button"
									on-tap="cancelDialog">
								cancel
							</paper-button>
						</div>
					</div>
					<div>
						<div>
							<datalist
									id="priceNames">
								<template
										is="dom-repeat"
										items="{{prices}}">
									<option value="{{item.name}}" />
								</template>
							</datalist>
							<input
									list="priceNames"
									autocomplete="on"
									placeholder="Name"
									value="{{priceName::input}}">
							</paper-input>
							<paper-tooltip>
								Name of the Product Offering Price.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									value="{{priceDescription}}"
									label="Description">
							</paper-input>
							<paper-tooltip>
								Description of the Product Offering Price.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
								type="datetime-local"
								label="Start Date"
								value="{{offerStartDatePrice}}">
							</paper-input>
							<paper-tooltip>
								Start of Product Offering Price validity period.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									type="datetime-local"
									label="End Date"
									value="{{offerEndDatePrice}}">
							</paper-input>
							<paper-tooltip>
								End of Product Offering Price validity period.
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									label="Price Type"
									value="{{priceType}}"
									no-animations="true"
									on-selected-item-changed="_checkRecurring">
								<paper-listbox
										id="addPriceType"
										slot="dropdown-content">
									<paper-item>
										Recurring
									</paper-item>
									<paper-item>
										One Time
									</paper-item>
									<paper-item>
										Usage
									</paper-item>
									<paper-item>
										Tariff
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Type of Product Offering Price.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									id="addPla"
									value="{{pricePla}}"
									input="url"
									pattern="https?://.+"
									label="Pricing Logic Algorithm"
									auto-validate>
							</paper-input>
							<paper-tooltip>
								Provide a URL to a remote (Class A) Rating Function (RF)
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									id="addPriceSize"
									label="Unit Size"
									value="{{priceSize}}"
									allowed-pattern="[0-9kmg]"
									pattern="^[0-9]+[kmg]?$"
									auto-validate>
							</paper-input>
							<paper-tooltip>
								Size of unit determines the granularity of rating.
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									label="Units"
									no-animations="true"
									value="{{priceUnits}}"
									on-selected-item-changed="_checkPattern">
								<paper-listbox
										id="addPriceUnits"
										slot="dropdown-content">
									<paper-item id="priceBytes">
											Bytes
									</paper-item>
									<paper-item id="priceCents">
											Cents
									</paper-item>
									<paper-item id="priceSeconds">
											Seconds
									</paper-item>
									<paper-item id="priceMessages">
											Messages
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Type of units to be rated.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									id="addPriceAmount"
									allowed-pattern="[0-9.]"
									pattern="[0-9]+\.?[0-9]{0,6}$"
									auto-validate
									label="Amount"
									value={{priceAmount}}>
							</paper-input>
							<paper-tooltip>
								Amount to charge for each unit.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									value="{{priceCurrency}}"
									label="Currency">
							</paper-input>
							<paper-tooltip>
								Currency used in Product Offering Price.
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									id="addPricePerioddrop"
									label="Period"
									value="{{pricePeriod}}"
									no-animations="true">
								<paper-listbox
										id="addPricePeriod"
										slot="dropdown-content"
										selected="2">
									<paper-item>
										Hourly
									</paper-item>
									<paper-item>
										Daily
									</paper-item>
									<paper-item>
										Weekly
									</paper-item>
									<paper-item>
										Monthly
									</paper-item>
									<paper-item>
										Yearly
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Period of recurring Product Offering Price.
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									value="{{priceAlteration}}"
									no-animations="true"
									label="Alteration">
								<paper-listbox
										id="addPriceAlteration"
										slot="dropdown-content">
									<template
											is="dom-repeat"
											items="[[alterations]]">
										<paper-item>
											{{item.name}}
										</paper-item>
									</template>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Bind an alteration to this Product Offering Price.
							</paper-tooltip>
						</div>
						<div>
							<span>Characteristics</span>
							<paper-icon-button
									id="priceCharsSection"
									suffix
									icon="arrow-drop-down"
									on-tap="_collapsePriceChars">
							</paper-icon-button>
						</div>
						<iron-collapse
								id="addPriceChars">
							<div>
								<span>Time of Day Range</span>
								<paper-icon-button
										id="priceCharsTimeSection"
										suffix
										icon="arrow-drop-down"
										on-tap="_collapsePriceCharsTime">
								</paper-icon-button>
							</div>
							<iron-collapse
									id="addPriceCharsTime">
								<paper-input
										type="time"
										label="Start Time"
										value="{{priceTodStart}}">
								</paper-input>
								<paper-tooltip>
									Time of day when this Product Offering Price starts to apply.
								</paper-tooltip>
								<paper-input
										type="time"
										label="End Time"
										value="{{priceTodEnd}}">
								</paper-input>
								<paper-tooltip>
									Time of day when this Product Offering Price ends applying.
								</paper-tooltip>
							</iron-collapse>
							<div>
								<span>Call Direction</span>
								<paper-icon-button
										id="callDirSection"
										suffix
										icon="arrow-drop-down"
										on-tap="_collapseCallDirection">
								</paper-icon-button>
							</div>
							<iron-collapse
									id="callDirection">
								<div>
									<paper-checkbox
											value="{{priceIncoming}}"> 
										Incoming
									</paper-checkbox>
								</div>
								<div>
									<paper-checkbox
											value="{{priceOutgoing}}"> 
										Outgoing
									</paper-checkbox>
								</div>
							</iron-collapse>
							<div>
								<paper-input
										id="addPriceCharReserveTime"
										allowed-pattern="[0-9mh]"
										pattern="^[0-9]+[mh]?$"
										auto-validate
										label="RADIUS Reserve Time"
										value="{{priceReserveTime}}">
								</paper-input>
								<paper-tooltip>
									Amount of time to reserve on RADIUS Accounting-Start and add to reported duration on Accounting-Interim.
									Character reserve time of price
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										id="addPriceCharReserveBytes"
										allowed-pattern="[0-9kmg]"
										pattern="^[0-9]+[kmg]?$"
										auto-validate
										label="RADIUS Reserve Data"
										value="{{priceReserveBytes}}">
								</paper-input>
								<paper-tooltip>
									Amount of bytes to reserve on RADIUS Accounting-Start and add to reported bytes on Accounting-Interim.
								</paper-tooltip>
							</div>
							<div>
								<paper-dropdown-menu
										id="destPrefixTariff"
										value="{{priceTariff}}"
										no-animations="true"
										label="Prefix Tariff Table">
									<paper-listbox
											slot="dropdown-content">
										<template
												is="dom-repeat"
												items="{{prefixTables}}">
											<paper-item>
												{{item.name}}
											</paper-item>
										</template>
									</paper-listbox>
								</paper-dropdown-menu>
								<paper-tooltip>
									Name of table for Tariff type Product Offering Price.
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										value="{{priceRoaming}}"
										label="Roaming Table">
								</paper-input>
								<paper-tooltip>
									Name of table for matching VPLMN.
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										type="number"
										value="{{priceKey}}"
										label="Charging Key">
								</paper-input>
								<paper-tooltip>
									Charging Key (Rating Group) specific to this Product Offering Price.
								</paper-tooltip>
							</div>
						</iron-collapse>
						<div
								class="buttons">
							<paper-button
									raised
									class="submit-button"
									on-tap="_addPrice">
								[[addOrUpdateButton]]
							</paper-button>
							<paper-button
									dialog-dismiss
									class="cancel-button"
									on-tap="cancelDialog">
								cancel
							</paper-button>
						</div>
					</div>
					<div>
						<div>
							<datalist
									id="alterations">
								<template
										is="dom-repeat"
										items="[[alterations]]">
									<option value="{{item.name}}" />
								</template>
							</datalist>
							<paper-input
									label="Name"
									list="alterations"
									value="{{alterationName}}"
									autocomplete="on"
									on-value-changed="updateAltsDialog">
							</paper-input>
							<paper-tooltip>
								Name of the Product Offering Price Alteration.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									value="{{alterationDescription}}"
									label="Description">
							</paper-input>
							<paper-tooltip>
								Descriptionof the Product Offering Price Alteration.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
								type="datetime-local"
								label="Start Date"
								value="{{alterationStartDate}}">
							</paper-input>
							<paper-tooltip>
								Start of Product Offering Price Alteration validity period.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									type="datetime-local"
									label="End Date"
									value="{{alterationEndDate}}">
							</paper-input>
							<paper-tooltip>
								End of Product Offering Price Alteration validity period.
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									label="Price Type"
									value="{{alterationType}}"
									no-animations="true"
									on-selected-item-changed="_checkRecurringAlt">
								<paper-listbox
										id="addAltType"
										slot="dropdown-content">
									<paper-item>
										Recurring
									</paper-item>
									<paper-item>
										One Time
									</paper-item>
									<paper-item>
										Usage
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Type of Product Offering Price Alteration.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									id="addAltSize"
									label="Unit Size"
									value="{{alterationSize}}"
									allowed-pattern="[0-9kmg]"
									pattern="^[0-9]+[kmg]?$"
									auto-validate>
							</paper-input>
							<paper-tooltip>
								Size of unit determines the granularity of rating.
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									label="Units"
									value="{{alterationUnit}}"
									on-selected-item-changed="_checkPatternAlt"
									no-animations="true">
								<paper-listbox
										id="addAltUnitDrop"
										slot="dropdown-content">
									<paper-item
											id="altBytes">
										Bytes
									</paper-item>
									<paper-item
											id="altCents">
										Cents
									</paper-item>
									<paper-item
											id="altSeconds">
										Seconds
									</paper-item>
									<paper-item
											id="altMessages">
										Messages
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Type of units to be rated.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									label="Amount"
									type="text"
									allowed-pattern="[0-9.]"
									pattern="[0-9]+\.?[0-9]{0,6}$"
									auto-validate
									value="{{alterationAmount}}">
							</paper-input>
							<paper-tooltip>
								Amount to charge for each unit.
							</paper-tooltip>
						</div>
						<div>
							<paper-input
									value="{{alterationCurrency}}"
									label="Currency">
							</paper-input>
							<paper-tooltip>
								Currency used in Product Offering Price Alteration.
							</paper-tooltip>
						</div>
						<div>
							<paper-dropdown-menu
									id="altPeriodDrop"
									label="Period"
									no-animations="true"
									value="{{alterationPeriod}}">
								<paper-listbox
										id="addAltPeriod"
										slot="dropdown-content">
									<paper-item>
										Hourly
									</paper-item>
									<paper-item>
										Daily
									</paper-item>
									<paper-item>
										Weekly
									</paper-item>
									<paper-item>
										Monthly
									</paper-item>
									<paper-item>
										Yearly
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip>
								Period of recurring Product Offering Price Alteration.
							</paper-tooltip>
						</div>
						<div
								class="buttons">
							<paper-button
									raised
									class="submit-button"
									on-tap="_addAlteration">
								[[addOrUpdateButton]]
							</paper-button>
							<paper-button
									dialog-dismiss
									class="cancel-button"
									on-tap="cancelDialog">
								cancel
							</paper-button>
						</div>
					</div>
				</iron-pages>
			</paper-dialog>
			<iron-ajax
					id="addOfferAjax"
					url="/catalogManagement/v2/productOffering"
					method="POST"
					content-type="application/json"
					on-loading-changed="_onLoadingChanged"
					on-response="_addOfferResponse"
					on-error="_addOfferError">
			</iron-ajax>
			<iron-ajax id="getPrefixTablesAjax"
					on-response="_getTariffTablesResponse"
					on-error="_getTariffTablesError">
			</iron-ajax>
			<iron-ajax id="getPolicyTablesAjax"
					on-response="_getPolicyTablesResponse"
					on-error="_getPolicyTablesError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			alterations: {
				type: Array,
				value: function() {
					return [];
				}
			},
			prices: {
				type: Array,
				value: function() {
					return [];
				}
			},
			prefixTables: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return [];
				}
			},
			policyTables: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return [];
				}
			},
			activePage: {
				type: Boolean,
				value: false,
				readOnly: true,
				observer: '_activePageChanged'
			},
			bundleOffers: {
				type: Array,
				value: function() {
					return [];
				}
			},
			selected: {
				type: Number,
				value: 0
			},
			addOrUpdateButton: {
				type: String,
				value: "add"
			},
			offerName: {
				type: String
			},
			offerDescription: {
				type: String
			},
			offerStartDate: {
				type: String
			},
			offerEndDate: {
				type: String
			},
			offerStartDatePrice: {
				type: String
			},
			offerEndDatePrice: {
				type: String
			},
			offerPolicy: {
				type: String
			},
			priceName: {
				type: String,
				observer: '_addUpdatePriceDialog'
			},
			priceDescription: {
				type: String
			},
			priceSize: {
				type: String
			},
			priceCurrency: {
				type: String
			},
			priceType: {
				type: String
			},
			pricepla: {
				type: String
			},
			priceUnits: {
				type: String
			},
			priceAmount: {
				type: String
			},
			priceAlteration: {
				type: String
			},
			priceTodStart: {
				type: String
			},
			priceIncoming: {
				type: Boolean
			},
			priceOutgoing: {
				type: Boolean
			},
			priceReserveTime: {
				type: Number,
				value: 0
			},
			priceReserveBytes: {
				type: Number,
				value: 0
			},
			priceTariff: {
				type: String
			},
			priceRoaming: {
				type: String
			},
			priceKey: {
				type: Number
			},
			alterationName: {
				type: String
			},
			alterationDescription: {
				type: String
			},
			alterationStartDate: {
				type: String
			},
			alterationEndDate: {
				type: String
			},
			alterationType: {
				type: String
			},
			alterationSize: {
				type: String
			},
			alterationAmount: {
				type: String
			},
			alterationCurrency: {
				type: String
			},
			redirectAddress: {
				type: String
			},
			serviceIdentifier: {
				type: Number
			}
		}
	}

	static get observers() {
		return [
			'_bundleCheckboxChanged(bundleOffers.*)',
			'_offersChanged(offers.splices)'
		]
	}

	ready() {
		super.ready();
	}

	_activePageChanged() {
		var grid = this.$.offerGrid;
		var ajax1 = this.$.getPrefixTablesAjax;
		ajax1.url = "/resourceInventoryManagement/v1/resource?resourceSpecification.id=1";
		ajax1.generateRequest();
		var ajax2 = this.$.getPolicyTablesAjax;
		ajax2.url = "/resourceInventoryManagement/v1/resource?resourceSpecification.id=3";
		ajax2.generateRequest();
	}

	_offersChanged(change) {
		if(change) {
			function doChange(splice) {
				function removeOffer(offerName) {
					function checkName(bundleOffer) {
						return bundleOffer.name == offerName;
					}
					var index = this.bundleOffers.findIndex(checkName);
					this.splice('bundleOffers', index, 1);
				}
				splice.removed.forEach(removeOffer, this);
				for (var i = 0; i < splice.addedCount; i++) {
					var checkOffer = {checked: false,
							name: splice.object[splice.index + i]};
					this.push('bundleOffers', checkOffer);
				}
			}
			change.indexSplices.forEach(doChange, this);
		}
	}

	_getPolicyTablesResponse(event) {
		var grid = this.$.offerGrid;
		var results = event.detail.xhr.response;
		this.splice("policyTables", 0, this.policyTables.length)
		for (var indexTable1 in results) {
			var tableRecord1 = new Object();
			tableRecord1.id = results[indexTable1].id;
			tableRecord1.href = results[indexTable1].href;
			tableRecord1.name = results[indexTable1].name;
			this.push('policyTables', tableRecord1);
		}
	}

	_getTariffTablesResponse(event) {
		var grid = this.$.offerGrid;
		var results = event.detail.xhr.response;
		this.splice("prefixTables", 0, this.prefixTables.length)
		for (var indexTable in results) {
			var tableRecord = new Object();
			tableRecord.id = results[indexTable].id;
			tableRecord.href = results[indexTable].href;
			tableRecord.name = results[indexTable].name;
			this.push('prefixTables', tableRecord);
		}
	}

	_addUpdatePriceDialog(event) {
		function checkPriceUpdateName(price) {
			return price.name == document.body.querySelector('sig-app').shadowRoot.getElementById('addOffer').priceName;
		}
		if(this.prices != undefined) {
			var indexPrice = this.prices.findIndex(checkPriceUpdateName);
			if (indexPrice == -1) {
				this.addOrUpdateButton = "add";
				this.priceDescription = null;
				this.offerStartDatePrice = null;
				this.offerEndDatePrice = null;
				this.priceType = null;
				this.pricePla = null;
				this.priceSize = null;
				this.priceUnits = null;
				this.priceAmount = null;
				this.priceCurrency = null;
				this.pricePeriod = null;
				this.priceAlteration = null;
				this.priceReserveTime = null;
				this.priceReserveBytes = null;
				this.priceTodStart = null;
				this.priceTodEnd = null;
			} else {
				this.addOrUpdateButton = "update";
				this.priceDescription = this.prices[indexPrice].description;
				this.offerStartDatePrice = this.prices[indexPrice].start;
				this.offerEndDatePrice = this.prices[indexPrice].end;
				switch(this.prices[indexPrice].type) {
					case "recurring":
						this.$.addPriceType.selected = 0;
						break;
					case "one_time":
						this.$.addPriceType.selected = 1;
						break;
					case "usage":
						this.$.addPriceType.selected = 2;
						break;
					case "tariff":
						this.$.addPriceType.selected = 3;
						break;
				}
				this.pricePla = this.prices[indexPrice].pla;
				this.priceSize = this.prices[indexPrice].size;
				switch(this.prices[indexPrice].unit) {
					case "b":
						this.$.addPriceUnits.selected = 0;
						break;
					case "c":
						this.$.addPriceUnits.selected = 1;
						break;
					case "s":
						this.$.addPriceUnits.selected = 2;
						break;
					case "msg":
						this.$.addPriceUnits.selected = 3;
						break;
				}
				this.priceCurrency = this.prices[indexPrice].currency;
				this.priceAmount = this.prices[indexPrice].amount;
				switch(this.prices[indexPrice].period) {
					case "hourly":
						this.$.addPricePeriod.selected = 0;
						break;
					case "daily":
						this.$.addPricePeriod.selected = 1;
						break;
					case "weekly":
						this.$.addPricePeriod.selected = 2;
						break;
					case "monthly":
						this.$.addPricePeriod.selected = 3;
						break;
					case "yearly":
						this.$.addPricePeriod.selected = 4;
						break;
				}
				if(this.prices[indexPrice].alterations &&
							this.prices[indexPrice].alterations.name) {
					var altPriceNew = this.prices[indexPrice].alterations.name;
					function checkAltName(alt) {
						return alt.name == altPriceNew;
					}
					this.$.addPriceAlteration.selected = this.alterations.findIndex(checkAltName);
				}
				this.priceReserveTime = this.prices[indexPrice].reserveTime;
				this.priceReserveBytes = this.prices[indexPrice].reserveBytes;
				this.priceTodStart = this.prices[indexPrice].timeOfDayRange.low.amount;
				this.priceTodEnd = this.prices[indexPrice].timeOfDayRange.up.amount;
				if(this.prices[indexPrice].callDirection == "answer") {
					this.priceIncoming = true;
				}
				if(this.prices[indexPrice].callDirection == "originate") {
					this.priceOutgoing = true;
				}
				this.priceTariff = this.prices[indexPrice].prefixTariff;
				this.priceRoaming = this.prices[indexPrice].roamingTable;
				this.priceKey = this.prices[indexPrice].priceKey;
			}
		}
	}

	updateAltsDialog() {
		function checkName(alt) {
			return alt.name == this.alterationName;
		}
		if(this.alterations != undefined) {
			var index = this.alterations.findIndex(checkName);
			if (index == -1) {
				this.addOrUpdateButton = "add";
				this.alterationDescription = null;
				this.alterationStartDate = null;
				this.alterationEndDate = null;
				this.alterationType = null;
				this.alterationSize = null;
				this.alterationUnit = null;
				this.alterationAmount = null;
				this.alterationCurrency = null;
				this.altAddPeriod = null;
			} else {
				this.addOrUpdateButton = "update";
				this.alterationDescription = this.alterations[index].description;
				this.alterationStartDate = this.alterations[index].startdate;
				this.alterationEndDate = this.alterations[index].terminationDate;
				switch(this.alterations[index].type) {
					case "recurring":
						this.$.addAltType.selected = 0;
						break;
					case "one_time":
						this.$.addAltType.selected = 1;
						break;
					case "usage":
						this.$.addAltType.selected = 2;
						break;
				}
				this.alterationSize = this.alterations[index].size;
				switch(this.alterations[index].unit) {
					case "b":
						this.$.addAltUnitDrop.selected = 0;
						break;
					case "c":
						this.$.addAltUnitDrop.selected = 1;
						break;
					case "s":
						this.$.addAltUnitDrop.selected = 2;
						break;
					case "msg":
						this.$.addAltUnitDrop.selected = 3;
						break;
				}
				this.alterationCurrency = this.alterations[index].currency;
				this.alterationAmount = this.alterations[index].amount;
				switch(this.alterations[index].period) {
					case "hourly":
						this.$.addAltPeriod.selected = 0;
						break;
					case "daily":
						this.$.addAltPeriod.selected = 1;
						break;
					case "weekly":
						this.$.addAltPeriod.selected = 2;
						break;
					case "monthly":
						this.$.addAltPeriod.selected = 3;
						break;
					case "yearly":
						this.$.addAltPeriod.selected = 4;
						break;
				}
			}
		}
	}

	_bundleCheckboxChanged() {
		function check(item) {
			return item.checked;
		}
		if(this.bundleOffers.some(check)) {
			this.$.addOfferProductSpecDrop.disabled = true;
		} else {
			this.$.addOfferProductSpecDrop.disabled = false;
		}
	}

	_onClickBundle() {
		if(this.$.addBundle.opened == false) {
			this.$.addBundle.show();
			this.$.onClickBundle.icon = "arrow-drop-up";
		} else {
			this.$.addBundle.hide();
			this.$.onClickBundle.icon = "arrow-drop-down";
		}
	}

	_onClickOfferChars() {
		if(this.$.addOfferChars.opened == false) {
			this.$.addOfferChars.show();
			this.$.onClickOfferChars.icon = "arrow-drop-up";
		} else {
			this.$.addOfferChars.hide();
			this.$.onClickOfferChars.icon = "arrow-drop-down";
		}
	}

	_addOffer(event) {
		var offerNew = new Object();
		if(this.offerName) {
			offerNew.name = this.offerName;
		}
		if(this.offerDescription) {
			offerNew.description = this.offerDescription;
		}
		if(this.$.addBundle) {
			var bundled = new Array();
			for(var index in this.bundleOffers) {
				if(this.bundleOffers[index].checked == true) {
					var bundleOffer = new Object();
					bundleOffer.numberRelOfferLowerLimit = 0;
					bundleOffer.numberRelOfferUpperLimit = 1;
					bundleOffer.numberRelOfferDefault = 1;
					var bundleObj = new Object();
					bundleObj.id = this.bundleOffers[index].id;
					bundleObj.href = this.bundleOffers[index].href;
					bundleObj.name = this.bundleOffers[index].name;
					bundleObj.bundledProductOfferingOption = bundleOffer;
					bundled.push(bundleObj);
				}
			}
			offerNew.bundledProductOffering = bundled;
		}
		if(this.offerAddSpec == "Prepaid Data") {
			var prodSpecData = new Object();
			prodSpecData.id = "8";
			prodSpecData.href = "/catalogManagement/v2/productSpecification/8";
			offerNew.productSpecification = prodSpecData;
		} else if(this.offerAddSpec == "Prepaid Voice") {
			var prodSpecVoice = new Object();
			prodSpecVoice.id = "9";
			prodSpecVoice.href = "/catalogManagement/v2/productSpecification/9";
			offerNew.productSpecification = prodSpecVoice;
		} else if(this.offerAddSpec == "Prepaid SMS") {
			var prodSpecSms = new Object();
			prodSpecSms.id = "11";
			prodSpecSms.href = "/catalogManagement/v2/productSpecification/11";
			offerNew.productSpecification = prodSpecSms;
		}
		var startDateTime;
		var endDateTime;
		if(this.offerStartDate) {
			startDateTime = this.offerStartDate;
		}
		if(this.offerEndDate) {
			endDateTime = this.offerEndDate;
		}
		if(startDateTime && endDateTime) {
			offerNew.validFor = {startDateTime, endDateTime};
		} else if(startDateTime && !endDateTime) {
			offerNew.validFor = {startDateTime};
		} else if(!startDateTime && endDateTime) {
			offerNew.validFor = {endDateTime};
		}
		if(this.offerAddStatus) {
			offerNew.lifecycleStatus = this.offerAddStatus;
		}
		var prodSpecCharValueUse = new Array();
		if (this.$.addOfferCharReserveSession.value && this.$.addOfferCharReserveSession.value.length > 0) {
			var charValueUse = new Object();
			charValueUse.name = "radiusReserveSessionTime";
			charValueUse.minCardinality = 0;
			charValueUse.maxCardinality = 1;
			var charValue = new Object();
			charValue.default = true;
			charValue.value = parseInt(this.$.addOfferCharReserveSession.value);
			var charValues = new Array();
			charValues.push(charValue);
			charValueUse.productSpecCharacteristicValue = charValues;
			var prodSpec = new Object();
			prodSpec.id = "1";
			prodSpec.href = "/catalogManagement/v2/productSpecification/1";
			charValueUse.productSpecification = prodSpec;
			prodSpecCharValueUse.push(charValueUse);
		}
		if(this.redirectAddress) {
			var redirect = new Object();
			redirect.name = "redirectServer"
			redirect.minCardinality = 0;
			redirect.maxCardinality = 1;
			var redirectUse = new Object();
			redirectUse.value = this.redirectAddress;
			var redirects = new Array();
			redirects.push(redirectUse);
			redirect.productSpecCharacteristicValue = redirects;
			var prodSpec1 = new Object();
			prodSpec1.id = "8";
			prodSpec1.href = "/productCatalogManagement/v2/productSpecification/8",
			redirect.productSpecification = prodSpec1;
			prodSpecCharValueUse.push(redirect);
		}
		if(this.serviceIdentifier) {
			var service = new Object();
			service.name = "serviceIdentifier"
			service.minCardinality = 0;
			service.maxCardinality = 1;
			var serviceUse = new Object();
			serviceUse.value = this.serviceIdentifier;
			var services = new Array();
			services.push(serviceUse);
			service.productSpecCharacteristicValue = services;
			var prodSpec1 = new Object();
			prodSpec1.id = "8";
			prodSpec1.href = "/productCatalogManagement/v2/productSpecification/8",
			service.productSpecification = prodSpec1;
			prodSpecCharValueUse.push(service);
		}
		if (this.offerPolicy) {
			var charValue = new Object();
			var charValueUse = new Object();
			charValueUse.name = "policyTable";
			charValueUse.minCardinality = 0;
			charValueUse.maxCardinality = 1;
			charValue.default = true;
			charValue.value = this.offerPolicy;
			var charValues = new Array();
			charValues.push(charValue);
			charValueUse.productSpecCharacteristicValue = charValues;
			var prodSpec = new Object();
			prodSpec.id = "3";
			prodSpec.href = "/catalogManagement/v2/productSpecification/4";
			charValueUse.productSpecification = prodSpec;
			prodSpecCharValueUse.push(charValueUse);
		}
		if (prodSpecCharValueUse.length > 0) {
			offerNew.prodSpecCharValueUse = prodSpecCharValueUse;
		}
		function mapPrice(item) {
			var out = new Object();
			if(item.name) {
				out.name = item.name;
			}
			if(item.description) {
				out.description = item.description;
			}
			var startDateTime;
			var endDateTime;
			if(item.start) {
				startDateTime = item.start;
			}
			if(item.end) {
				endDateTime = item.end;
			}
			if(startDateTime && endDateTime) {
				out.validFor = {startDateTime, endDateTime};
			} else if(startDateTime && !endDateTime) {
				out.validFor = {startDateTime};
			} else if(!startDateTime  && endDateTime) {
				out.validFor = {endDateTime};
			}
			if(item.type) {
				out.priceType = item.type;
			}
			if(item.pla) {
				var plas = new Array();
				var pla = new Object();
				pla.href = item.pla;
				plas.push(pla);
				out.pricingLogicAlgorithm = plas;
			}
			if(item.unit && item.size) {
				var len = item.size.length;
				var m = item.size.charAt(len - 1);
				if(isNaN(parseInt(m))) {
					var s = item.size.slice(0, (len - 1));
				} else {
					var s = item.size;
				}
				if(item.unit == "b") {
					if (m == "m") {
						out.unitOfMeasure = s + "000000b";
					} else if(m == "g") {
						out.unitOfMeasure = s + "000000000b";
					} else if(m == "k") {
						out.unitOfMeasure = s + "000b";
					} else {
						out.unitOfMeasure = s + "b";
					}
				} else if(item.unit == "s") {
					var n = Number(s);
					if(m == "m") {
						n = n * 60;
						out.unitOfMeasure = n.toString() + "s";
					} else if(m == "h") {
						n = n * 3600;
						out.unitOfMeasure = n.toString() + "s";
					} else {
						out.unitOfMeasure = n.toString() + "s";
					}
				} else if(item.unit == "msg") {
					out.unitOfMeasure = s + "msg";
				}
			}
			var taxIncludedAmount;
			var currencyCode;
			if(item.amount) {
				taxIncludedAmount = item.amount;
			}
			if(item.currency) {
				currencyCode = item.currency;
			}
			if(item.amount && item.currency) {
				out.price = {taxIncludedAmount, currencyCode};
			} else if (item.amount && !item.currency) {
				out.price = {taxIncludedAmount};
			} else if (!item.amount && item.currency) {
				out.price = {currencyCode};
			}
			if(item.period) {
				out.recurringChargePeriod = item.period;
			}
			if(item.alterations) {
				out.productOfferPriceAlteration = new Object();
				if(item.alterations.name) {
					out.productOfferPriceAlteration.name = item.alterations.name;
				}
				if (item.alterations.description) {
					out.productOfferPriceAlteration.description = item.alterations.description;
				}
				var startDateTime;
				var endDateTime;
				if (item.alterations.startdate) {
					startDateTime = item.alterations.startdate;
				}
				if (item.alterations.terminationDate) {
					endDateTime = item.alterations.terminationDate;
				}
				if (startDateTime && endDateTime) {
					out.productOfferPriceAlteration.validFor = {startDateTime, endDateTime}
				} else if(startDateTime && !endDateTime) {
					out.productOfferPriceAlteration.validFor = {startDateTime};
				} else if(!startDateTime && endDateTime) {
					out.productOfferPriceAlteration.validFor = {endDateTime};
				}
				if (item.alterations.type) {
					out.productOfferPriceAlteration.priceType = item.alterations.type;
				}
				if (item.alterations.unit && item.alterations.size) {
					var len1 = item.alterations.size.length;
					var m1 = item.alterations.size.charAt(len1 - 1);
					if(isNaN(parseInt(m1))) {
						var s1 = item.alterations.size.slice(0, (len1 - 1));
					} else {
						var s1 = item.alterations.size;
					}
					if(item.alterations.unit == "b") {
						if (m1 == "m") {
							out.productOfferPriceAlteration.unitOfMeasure = s1 + "000000b";
						} else if(m1 == "g") {
						out.productOfferPriceAlteration.unitOfMeasure = s1 + "000000000b";
						} else if(m1 == "k") {
							out.productOfferPriceAlteration.unitOfMeasure = s1 + "000b";
						} else {
							out.productOfferPriceAlteration.unitOfMeasure = s1 + "b";
						}
					} else if(item.alterations.unit == "s") {
						var n1 = Number(s1);
						if(m1 == "m") {
							n1 = n1 * 60;
							out.productOfferPriceAlteration.unitOfMeasure = n1.toString() + "s";
						} else if(m1 == "h") {
							n1 = n1 * 3600;
							out.productOfferPriceAlteration.unitOfMeasure = n1.toString() + "s";
						} else {
							out.productOfferPriceAlteration.unitOfMeasure = n1.toString() + "s"; 
						}
					}
				}
				var taxIncludedAmount;
				var currencyCode;
				if (item.alterations.amount || item.alterations.amount == 0) {
					taxIncludedAmount = item.alterations.amount;
				}
				if (item.alterations.currency) {
					currencyCode = item.alterations.currency;
				}
				if((taxIncludedAmount >= 0) && currencyCode) {
					out.productOfferPriceAlteration.price = {taxIncludedAmount, currencyCode};
				} else if ((taxIncludedAmount >= 0) && !currencyCode) {
					out.productOfferPriceAlteration.price = {taxIncludedAmount};
				} else if (!(taxIncludedAmount >= 0) && currencyCode) {
					out.productOfferPriceAlteration.price = {currencyCode};
				}
				if (item.alterations.period) {
					out.productOfferPriceAlteration.recurringChargePeriod = item.alterations.period;
				}
			}
			var prodSpecCharValueUse = new Array();
			if (item.prefixTariff) {
				var charValue = new Object();
				var charValueUse = new Object();
				charValueUse.name = "destPrefixTariffTable";
				charValueUse.minCardinality = 0;
				charValueUse.maxCardinality = 1;
				charValue.default = true;
				charValue.value = item.prefixTariff;
				var charValues = new Array();
				charValues.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValues;
				var prodSpec = new Object();
				prodSpec.id = "3";
				prodSpec.href = "/catalogManagement/v2/productSpecification/3";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
			}
			if (item.roamingTable) {
				var charValue = new Object();
				var charValueUse = new Object();
				charValueUse.name = "roamingTable";
				charValueUse.minCardinality = 0;
				charValueUse.maxCardinality = 1;
				charValue.default = true;
				charValue.value = item.roamingTable;
				var charValues = new Array();
				charValues.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValues;
				var prodSpec = new Object();
				prodSpec.id = "3";
				prodSpec.href = "/catalogManagement/v2/productSpecification/3";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
			}
			if (item.chargingKey) {
				var charValue = new Object();
				var charValueUse = new Object();
				charValueUse.name = "chargingKey";
				charValue.value = parseInt(item.chargingKey);
				var charValues = new Array();
				charValues.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValues;
				var prodSpec = new Object();
				prodSpec.id = "3";
				prodSpec.href = "/catalogManagement/v2/productSpecification/3";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
			}
			if (item.reserveTime) {
				var charValue = new Object();
				charValue.unitOfMeasure = "seconds";
				var charLength = item.reserveTime.length;
				var lastChar = item.reserveTime.charAt(charLength - 1);
				if(isNaN(parseInt(lastChar))) {
					var s = item.reserveTime.slice(0, (charLength -1));
				} else {
					var s = item.reserveTime;
				}
				if(charValue.unitOfMeasure == "seconds") {
					var n = Number(s);
					if(lastChar == "m") {
						charValue.value = n * 60;
					}
					else if(lastChar == "h") {
						charValue.value = n * 3600;
					} else {
						charValue.value = n;
					}
				}
				var charValueUse = new Object();
				charValueUse.name = "radiusReserveTime";
				charValueUse.valueType = "Number";
				charValueUse.minCardinality = 1;
				charValueUse.maxCardinality = 1;
				charValue.default = true;
				var charValues = new Array();
				charValues.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValues;
				var prodSpec = new Object();
				prodSpec.id = "4";
				prodSpec.href = "/catalogManagement/v2/productSpecification/4";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
			}
			if (item.reserveBytes) {
				var charValue = new Object();
				charValue.unitOfMeasure = "octets";
				var charLength = item.reserveBytes.length;
				var lastChar = item.reserveBytes.charAt(charLength - 1);
				if(isNaN(parseInt(lastChar))) {
					var s = item.reserveBytes.slice(0, (charLength -1));
				} else {
					var s = item.reserveBytes;
				}
				if(charValue.unitOfMeasure == "octets") {
					var n = Number(s);
					if(lastChar == "g") {
						charValue.value = n * 1000000000;
					}
					else if(lastChar == "m") {
						charValue.value = n * 1000000;
					}
					else if(lastChar == "k") {
						charValue.value = n * 1000;
					} else {
						charValue.value = n;
					}
				}
				var charValueUse = new Object();
				charValueUse.name = "radiusReserveOctets";
				charValueUse.valueType = "Number";
				charValueUse.minCardinality = 1;
				charValueUse.maxCardinality = 1;
				charValue.default = true;
				var charValuesByte = new Array();
				charValuesByte.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValuesByte;
				var prodSpec = new Object();
				prodSpec.id = "4";
				prodSpec.href = "/catalogManagement/v2/productSpecification/4";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
			}
			if (item.timeOfDayRange) {
				var charValueUse = new Object();
				charValueUse.name = "timeOfDayRange";
				charValueUse.valueType = "Range";
				charValueUse.minCardinality = 0;
				charValueUse.maxCardinality = 1;
				var charLowUpObj = new Object();
				var charValueFinal = new Object();
				var charValueLower = new Object();
				charValueLower.amount = item.timeOfDayRange.low.amount;
				charValueLower.units = "hours";
				charLowUpObj.lowerValue = charValueLower;
				var charValueUpper = new Object();
				charValueUpper.amount = item.timeOfDayRange.up.amount;
				charValueUpper.units = "hours";
				charLowUpObj.upperValue = charValueUpper;
				charValueFinal.value = charLowUpObj;
				var charValues = new Array();
				charValues.push(charValueFinal);
				charValueUse.productSpecCharacteristicValue = charValues;
				var prodSpec = new Object();
				prodSpec.id = "4";
				prodSpec.href = "/catalogManagement/v2/productSpecification/4";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
			}
			if (item.callDirection) {
				var charValue = new Object();
				var charValueUse = new Object();
				charValueUse.name = "callDirection";
				charValueUse.minCardinality = 1;
				charValueUse.maxCardinality = 1;
				charValue.default = true;
				charValue.value = item.callDirection; 
				var charValuesByte = new Array();
				charValuesByte.push(charValue);
				charValueUse.productSpecCharacteristicValue = charValuesByte;
				var prodSpec = new Object();
				prodSpec.id = "5";
				prodSpec.href = "/catalogManagement/v2/productSpecification/5";
				charValueUse.productSpecification = prodSpec;
				prodSpecCharValueUse.push(charValueUse);
			}
			if (prodSpecCharValueUse.length > 0) {
				out.prodSpecCharValueUse = prodSpecCharValueUse;
			}
			return out;
		}
		offerNew.productOfferingPrice = this.prices.map(mapPrice);
		if(offerNew.name) {
			var ajax = this.$.addOfferAjax;
			ajax.body = offerNew;
			ajax.generateRequest();
			this.$.addBundle.hide();
		} else {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
		}
	}

	_checkProductSpec() {
		if(this.offerAddSpec == "Prepaid Data") {
			this.$.destPrefixTariff.disabled = true;
		} else if(this.offerAddSpec == "Prepaid Voice") {
			this.$.destPrefixTariff.disabled = false;
		} else if(this.offerAddSpec == "Prepaid SMS") {
			this.$.destPrefixTariff.disabled = false;
		}
	}

	_checkPattern() {
		if(this.priceUnits == "Bytes") {
			this.$.addPriceSize.allowedPattern = "[0-9kmg]";
			this.$.addPriceSize.pattern = "^[0-9]+[kmg]?$";
			this.$.addPriceSize.disabled = false;
			this.$.addPriceCharReserveBytes.allowedPattern = "[0-9kmg]";
			this.$.addPriceCharReserveBytes.pattern = "^[0-9]+[kmg]?$";
			this.$.addPriceCharReserveBytes.disabled = false;
			this.$.addPriceCharReserveTime.disabled = true;
		} else if(this.priceUnits == "Cents") {
			this.$.addPriceSize.allowedPattern = "[0-9]";
			this.$.addPriceSize.pattern = "^[0-9]+$";
			this.$.addPriceSize.disabled = true;
		} else if(this.priceUnits == "Seconds") {
			this.$.addPriceSize.allowedPattern = "[0-9mh]";
			this.$.addPriceSize.pattern = "^[0-9]+[mh]?$";
			this.$.addPriceSize.disabled = false;
			this.$.addPriceCharReserveTime.allowedPattern = "[0-9mh]";
			this.$.addPriceCharReserveTime.pattern = "^[0-9]+[mh]?$";
			this.$.addPriceCharReserveTime.disabled = false;
			this.$.addPriceCharReserveBytes.disabled = true;
		} else if(this.priceUnits == "Messages") {
			this.$.addPriceSize.allowedPattern = "[0-9]";
			this.$.addPriceSize.pattern = "^[0-9]+$";
			this.$.addPriceSize.disabled = false;
		}
	}

	_checkPatternAlt() {
		if(this.alterationUnit == "Bytes") {
			this.$.addAltSize.allowedPattern = "[0-9kmg]";
			this.$.addAltSize.pattern = "^[0-9]+[kmg]?$";
			this.$.addAltSize.disabled = false;
		} 
		if(this.alterationUnit == "Cents") {
			this.$.addAltSize.allowedPattern = "[0-9]";
			this.$.addAltSize.pattern = "^[0-9]+$";
			this.$.addAltSize.disabled = true;
		}
		if(this.alterationUnit == "Seconds") {
			this.$.addAltSize.allowedPattern = "[0-9mh]";
			this.$.addAltSize.pattern = "^[0-9]+[mh]?$";
			this.$.addAltSize.disabled = false;
		}
		if(this.alterationUnit == "Messages") {
			this.$.addAltSize.allowedPattern = "[0-9]";
			this.$.addAltSize.pattern = "^[0-9]+$";
			this.$.addAltSize.disabled = false;
		}
	}

	_checkRecurring() {
		if(this.priceType == "Recurring") {
			this.$.addPricePerioddrop.disabled = false;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = true;
			this.$.priceMessages.disabled = true;
			this.$.priceCents.disabled = false;
			this.$.addPriceCharReserveTime.disabled = true;
			this.$.addPriceCharReserveBytes.disabled = true;
			this.$.addPriceUnits.selected = 1;
			this.$.addPriceAmount.disabled = false;
			this.$.addPla.disabled = true;
		} else if(this.priceType == "One Time") {
			this.$.addPricePerioddrop.disabled = true;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = true;
			this.$.priceMessages.disabled = true;
			this.$.priceCents.disabled = false;
			this.$.addPriceCharReserveTime.disabled = true;
			this.$.addPriceCharReserveBytes.disabled = true;
			this.$.addPriceUnits.selected = 1;
			this.$.addPriceAmount.disabled = false;
			this.$.addPla.disabled = true;
		} else if(this.priceType == "Usage") {
			this.$.addPricePerioddrop.disabled = true;
			this.$.priceCents.disabled = true;
			this.$.priceMessages.disabled = false;
			this.$.priceBytes.disabled = false;
			this.$.priceSeconds.disabled = false;
			this.$.addPriceUnits.selected = 0;
			this.$.addPriceAmount.disabled = false;
			this.$.addPla.disabled = true;
		} else if(this.priceType == "Tariff") {
			this.$.addPricePerioddrop.disabled = true;
			this.$.priceCents.disabled = true;
			this.$.priceBytes.disabled = true;
			this.$.priceMessages.disabled = false;
			this.$.priceSeconds.disabled = false;
			this.$.addPriceUnits.selected = 2;
			this.$.addPriceAmount.disabled = true;
			this.$.addPla.disabled = false;
			this.priceAmount = null;
		}
	}

	_checkRecurringAlt() {
		if(this.alterationType == "Recurring") {
			this.$.altPeriodDrop.disabled = false;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = true;
			this.$.altMessages.disabled = false;
			this.$.addAltUnitDrop.selected = 0;
		} else if(this.alterationType == "One Time") {
			this.$.altPeriodDrop.disabled = true;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = false;
			this.$.altMessages.disabled = false;
			this.$.addAltUnitDrop.selected = 1;
		} else if(this.alterationType == "Usage") {
			this.$.altPeriodDrop.disabled = true;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = true;
			this.$.altMessages.disabled = false;
		}
	}
	_addPrice(event) {
		function checkPriceName(price) {
			return price.name == null;
		}
		var indexPrice = this.prices.findIndex(checkPriceName);
		if (indexPrice == -1) {
			var priceNew = new Object();
		} else {
			var priceNew = this.prices[indexPrice];
		}
		priceNew.name = this.priceName;
		priceNew.description = this.priceDescription;
		priceNew.start = this.offerStartDatePrice;
		priceNew.end = this.offerEndDatePrice;
		switch(this.$.addPriceType.selected) {
			case 0:
				priceNew.type = "recurring";
				break;
			case 1:
				priceNew.type = "one_time";
				break;
			case 2:
				priceNew.type = "usage";
				break;
			case 3:
				priceNew.type = "tariff";
				break;
		}
		priceNew.pla = this.pricePla;
		switch(this.$.addPriceUnits.selected) {
			case 0:
				priceNew.unit = "b";
				break;
			case 1:
				priceNew.unit = "c";
				break;
			case 2:
				priceNew.unit = "s";
				break;
			case 3:
				priceNew.unit = "msg";
				break;
		}
		if (this.priceAmount) {
			priceNew.amount = this.priceAmount;
		}
		priceNew.size = this.priceSize;
		priceNew.currency = this.priceCurrency;
		switch(this.$.addPricePeriod.selected) {
			case 0:
				priceNew.period = "hourly";
				break;
			case 1:
				priceNew.period = "daily";
				break;
			case 2:
				priceNew.period = "weekly";
				break;
			case 3:
				priceNew.period = "monthly";
				break;
			case 4:
				priceNew.period = "yearly";
				break;
		}
		var priAltObj = this.priceAlteration;
		if(this.priceAlteration) {
			function checkAlt(alts) {
				return alts.name == priAltObj;
			}
			var altIndex = this.alterations.findIndex(checkAlt);
			priceNew.alterations = this.alterations[altIndex];
		}
		priceNew.reserveTime = this.priceReserveTime;
		priceNew.reserveBytes = this.priceReserveBytes;
		var dayStartTime = this.priceTodStart;
		var dayEndTime = this.priceTodEnd;
		if(dayStartTime && dayEndTime ) {
			var low = new Object();
			low.amount = dayStartTime;
			low.units = "hours";
			var up = new Object();
			up.amount = dayEndTime;
			up.units = "hours";
			priceNew.timeOfDayRange = {low, up};
		}
		if(this.priceIncoming == true) {
			priceNew.callDirection = "answer";
		}
		if(this.priceOutgoing == true) {
			priceNew.callDirection = "originate";
		}
		priceNew.prefixTariff = this.priceTariff;
		priceNew.roamingTable = this.priceRoaming;
		priceNew.chargingKey = parseInt(this.priceKey);
		if(priceNew.name
					&& priceNew.type
					&& priceNew.unit
					&& (priceNew.size || priceNew.unit == "c")
					&& (priceNew.amount || priceNew.type == "tariff")
					&& (priceNew.type == "recurring" && priceNew.period)
					|| (priceNew.type == "one_time" || priceNew.period)
					|| (priceNew.type == "usage" || priceNew.period)
					|| (priceNew.type == "tariff" || priceNew.period)) {
			if (indexPrice == -1) {
				this.push('prices', priceNew);
			} else {
				this.splice('prices', indexPrice, 1, priceNew);
				this.addOrUpdateButton = "add";
			}
			this.priceName = null;
			this.priceDescription = null;
			this.offerStartDatePrice = null;
			this.offerEndDatePrice = null;
			this.$.addPriceType.selected = null;
			this.priceSize = null;
			this.$.addPriceUnits.selected = null;
			this.priceAmount = null;
			this.priceCurrency = null;
			this.$.addPricePeriod.selected = null;
			this.$.addPriceAlteration.selected = null;
			this.priceReserveTime = null;
			this.priceReserveBytes = null;
			this.priceTariff = null;
			this.priceRoaming = null;
			this.priceKey = null;
			this.priceTodStart = null;
			this.priceTodEnd = null;
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Success";
			toast.open();
		} else {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
		}
	}

	_collapsePriceChars() {
		if(this.$.addPriceChars.opened == false) {
			this.$.addPriceChars.show();
			this.$.priceCharsSection.icon = "arrow-drop-up";
		} else {
			this.$.addPriceChars.hide();
			this.$.priceCharsSection.icon = "arrow-drop-down";
		}
	}

	_collapsePriceCharsTime() {
		if(this.$.addPriceCharsTime.opened == false) {
			this.$.addPriceCharsTime.show();
			this.$.priceCharsTimeSection.icon = "arrow-drop-up";
		} else {
			this.$.addPriceCharsTime.hide();
			this.$.priceCharsSection.icon = "arrow-drop-down";
		}
	}

	_collapseCallDirection() {
		if(this.$.callDirection.opened == false) {
			this.$.callDirection.show();
			this.$.callDirSection.icon = "arrow-drop-up";
		} else {
			this.$.callDirection.hide();
			this.$.callDirSection.icon = "arrow-drop-down";
		}
	}

	_addAlteration(event) {
		function checkAltName(alt) {
			return alt.name == this.alterationName;
		}
		var indexAlt = this.alterations.findIndex(checkAltName);
		if (indexAlt == -1) {
			var altNew = new Object();
		} else {
			var altNew = this.prices[indexAlt];
		}
		var altNew = new Object();
		altNew.name = this.alterationName;
		altNew.description = this.alterationDescription;
		altNew.startdate = this.alterationStartDate;
		altNew.terminationDate = this.alterationEndDate;
		switch(this.$.addAltType.selected) {
			case 0:
				altNew.type = "recurring";
				break;
			case 1:
				altNew.type = "one_time";
				break;
			case 2:
				altNew.type = "usage";
				break;
		}
		altNew.size = this.alterationSize;
		if(this.alterationAmount) {
			altNew.amount= this.alterationAmount;
		}
		altNew.currency = this.alterationCurrency;
		switch(this.$.addAltPeriod.selected) {
			case 0:
				altNew.period = "hourly";
				break;
			case 1:
				altNew.period = "daily";
				break;
			case 2:
				altNew.period = "weekly";
				break;
			case 3:
				altNew.period = "monthly";
				break;
			case 4:
				altNew.period = "yearly";
				break;
		}
		switch(this.$.addAltUnitDrop.selected) {
			case 0:
				altNew.unit = "b";
				break;
			case 1:
				altNew.unit = "c";
				break;
			case 2:
				altNew.unit = "s";
				break;
			case 3:
				altNew.unit = "msg";
				break;
		}
		if(altNew.name
					&& altNew.type
					&& altNew.unit
					&& (altNew.size || altNew.unit == "c")
					&& (altNew.amount || altNew.amount == 0)
					&& (altNew.type == "recurring" && altNew.period)
               || (altNew.type == "one_time" || altNew.period)
               || (altNew.type == "usage" || altNew.period)) {
			if (indexAlt == -1) {
				this.push('alterations', altNew);
			} else {
				this.splice('alterations', indexAlt, 1, altNew);
				this.addOrUpdateButton = "add";
			}
			this.alterationName = null;
			this.alterationDescription = null;
			this.alterationStartDate = null;
			this.alterationEndDate = null;
			this.$.addAltType.selected = null;
			this.alterationSize = null;
			this.$.addAltUnitDrop.selected = null;
			this.alterationAmount = null;
			this.alterationCurrency = null;
			this.$.addAltPeriod.selected = null;
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Success";
			toast.open();
		} else {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
		}
	}

	_addOfferResponse(event) {
		this.$.addOfferModal.close();
		this.offerName = null;
		this.offerDescription = null;
		this.set('prices', []);
		this.set('alterations', []);
		document.body.querySelector('sig-app').shadowRoot.getElementById('offerList').shadowRoot.getElementById('offerGrid').clearCache();
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		this.cancelDialog();
	}

	_addOfferError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	cancelDialog() {
		this.set('prices', []);
		this.set('alterations', []);
		this.addOrUpdateButton = "add";
		this.offerName = null;
		this.offerDescription = null;
		this.$.addOfferProductSpecList.selected = null;
		this.$.addOfferChars.hide();
		this.$.addBundle.hide();
		this.priceTariff = null;
		this.priceRoaming = null;
		this.priceKey = null;
		this.$.addOfferCharReserveSession.value = null;
		this.offerStartDate = null;
		this.offerEndDate = null;
		this.offerStartDatePrice = null;
		this.offerEndDatePrice = null;
		this.priceName = null;
		this.priceDescription = null;
		this.$.addPriceType.selected = null;
		this.priceSize = null;
		this.$.addPriceUnits.selected = null;
		this.priceAmount = null;
		this.priceCurrency = null;
		this.$.addPricePeriod.selected = null;
		this.priceReserveTime = null;
		this.priceReserveBytes = null;
		this.alterationName = null;
		this.alterationDescription = null;
		this.alterationStartDate = null;
		this.alterationEndDate = null;
		this.$.addAltType.selected = null;
		this.alterationSize = null;
		this.$.addAltUnitDrop.selected = null;
		this.alterationAmount = null;
		this.alterationCurrency = null;
		this.$.addAltPeriod.selected = null;
		this.priceTodStart = null;
		this.priceTodEnd = null;
		this.$.addOfferModal.close();
	}

	_onLoadingChanged(event) {
		if (this.$.addOfferAjax.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
}

window.customElements.define('sig-offer-add', offerAdd);
