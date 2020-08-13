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
import './style-element.js';

class offerAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addProductModal" modal>
				<app-toolbar>
					<paper-tabs selected="{{selected}}">
						<paper-tab id="offer-add">
							<h2>Offering</h2>
						</paper-tab>
						<paper-tab id="price-add">
							<h2>Prices</h2>
						</paper-tab>
						<paper-tab id="alt-add">
							<h2>Alterations</h2>
						</paper-tab>
					</paper-tabs>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<iron-pages
						selected="{{selected}}">
					<div id="addOff-tab">
						<div>
							<paper-input
									id="addOffName"
									name="name"
									label="Name"
									value="{{offerAddAddress}}">
							</paper-input>
						</div>
						<div>
							<paper-input id="addOffDesc"
									name="description"
									label="Descrtiption"
									value="{{offerAddDes}}">
							</paper-input>
						</div>
						<div>
							<span>Bundled Products</span>
							<paper-icon-button
									id="onClickBundle"
									suffix
									icon="arrow-drop-down"
									on-click="_onClickBundle">
							</paper-icon-button>
						</div>
						<iron-collapse id="addBundle">
							<template is=dom-repeat items="{{offers}}">
								<div>
									<paper-checkbox checked="{{item.checked}}"> 
											{{item.name}}
									</paper-checkbox>
								</div>
							</template>
						</iron-collapse>
						<div>
							<paper-dropdown-menu
									id="addOffProductSpecificationDrop"
									on-selected-item-changed="checkProductSpec"
									value="{{offerAddSpec}}"
									label="Product Specification">
								<paper-listbox
										id="addOffProductSpecification"
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
						</div>
						<div>
							<paper-input
									id="addOffStart"
									value="{{addProductStartDateOffer}}"
									name="addProductStartDateOffer"
									label="Start Date"
							</paper-input>
						</div>
						<div>
							<paper-input id="addOffEnd"
									value="{{addProductEndDateOffer}}"
									name="addProductEndDateOffer"
									label="End Date">
							</paper-input>
						</div>
						<div>
							<span>Characteristics</span>
							<paper-icon-button
									id="onClickOfferChars"
									suffix
									icon="arrow-drop-down"
									on-click="_onClickOfferChars">
							</paper-icon-button>
						</div>
						<iron-collapse id="addOfferChars">
							<div>
								<paper-input
										id="addOfferCharReserveSession"
										allowed-pattern="[0-9mh]"
										pattern="^[0-9]+[mh]?$"
										auto-validate
										label="RADIUS Reserve Session Time"
										value=0>
								</paper-input>
							</div>
						</iron-collapse>
						<div class="buttons">
							<paper-button
									raised
									class="submit-button"
									on-tap="addOffer">
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
					<div id="addPrice-tab">
						<div>
							<datalist id="price">
								<template is="dom-repeat" items="[[prices]]">
									<option value="{{item.name}}" />
								</template>
							</datalist>
							<paper-input
									id="addPriceName"
									list="price"
									label="Name"
									value="{{priceAddName}}"
									on-value-changed="addUpdatePriceDialog">
							</paper-input>
						</div>
						<div>
							<paper-input
									id="addPriceDesc"
									name="description"
									value="{{priceAddDes}}"
									label="Description">
							</paper-input>
						</div>
						<div>
							<paper-input
								id="addPriceStartDate"
								value="{{addProductStartDatePrice}}"
								name="addProductStartDatePrice"
								label="Start Date">
							</paper-input>
						</div>
						<div>
							<paper-input id="addPriceEndDate"
									value="{{addProductEndDatePrice}}"
									name="addProductEndDatePrice"
									label="End Date">
							</paper-input>
						</div>
						<div>
							<paper-dropdown-menu
									id="addPriceTypedrop"
									label="Price Type"
									value="{{priceAddType}}"
									on-selected-item-changed="checkRecure">
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
						</div>
						<div>
							<paper-input
									id="addPriceSize"
									name="size"
									value="{{priceAddSize}}"
									type="text"
									allowed-pattern="[0-9kmg]"
									pattern="^[0-9]+[kmg]?$"
									label="Unit Size"
									auto-validate>
							</paper-input>
						</div>
						<div>
							<paper-dropdown-menu
									id="addPriceUnitsdrop"
									value="{{priceAddUnits}}"
									label="Units"
									on-selected-item-changed="checkPattern">
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
						</div>
						<div>
							<paper-input
									id="addPriceAmount"
									name="amount"
									type="text"
									allowed-pattern="[0-9.]"
									pattern="[0-9]+\.?[0-9]{0,6}$"
									auto-validate
									label="Amount"
									value=0>
							</paper-input>
						</div>
						<div>
							<paper-input
									id="addPriceCurrency"
									name="currency"
									value="{{priceAddCurrency}}"
									label="Currency">
							</paper-input>
						</div>
						<div>
							<paper-dropdown-menu
									id="addPricePerioddrop"
									value="{{priceAddPeriod}}"
									label="Period">
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
						</div>
						<div>
							<paper-dropdown-menu
									id="addPriceDrop"
									value="{{priceAddAlter}}"
									label="Alteration">
								<paper-listbox
										id="addPriceAlteration"
										slot="dropdown-content">
									<template is="dom-repeat" items="[[alterations]]">
										<paper-item>
											{{item.name}}
										</paper-item>
									</template>
								</paper-listbox>
							</paper-dropdown-menu>
						</div>
						<div>
							<span>Characteristics</span>
							<paper-icon-button
									id="onClickPriceChars"
									suffix
									icon="arrow-drop-down"
									on-click="_onClickPriceChars">
							</paper-icon-button>
						</div>
						<iron-collapse id="addPriceChars">
							<div>
								<span>Time of day range</span>
								<paper-icon-button
										id="onClickPriceCharsTime"
										suffix
										icon="arrow-drop-down"
										on-click="_onClickPriceCharsTime">
								</paper-icon-button>
							</div>
							<iron-collapse id="addPriceCharsTime">
								<paper-input
										id="timeOfDayStart"
										value="{{startTime}}"
										name="addProductTimePriceStart"
										label="Start Time">
								</paper-input>
								<paper-input
										id="timeOfDayEnd"
										value="{{endTime}}"
										name="addProductTimePriceEnd"
										label="End Time">
								</paper-input>
							</iron-collapse>
							<div>
								<span>Call Direction</span>
								<paper-icon-button
										id="onClickCall"
										suffix
										icon="arrow-drop-down"
										on-click="_onClickCall">
								</paper-icon-button>
							</div>
							<iron-collapse id="addCall">
								<div>
									<paper-checkbox
											id="checkIn"
											value="{{priceCheckIn}}"> 
										Incoming
									</paper-checkbox>
								</div>
								<div>
									<paper-checkbox
											id="checkOut"
											value="{{priceCheckIn}}"> 
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
										value=0>
								</paper-input>
							</div>
							<div>
								<paper-input
										id="addPriceCharReserveBytes"
										allowed-pattern="[0-9kmg]"
										pattern="^[0-9]+[kmg]?$"
										auto-validate
										label="RADIUS Reserve Data"
										value=0>
								</paper-input>
							</div>
							<div>
								<paper-input
										id="destPrefixTariff"
										type="string"
										value="{{priceAddTariff}}"
										label="Prefix Tariff Table">
								</paper-input>
							</div>
							<div>
								<paper-input
										id="roamingTable"
										type="string"
										value="{{priceAddRoaming}}"
										label="Roaming Table">
								</paper-input>
							</div>
						</iron-collapse>
						<div class="buttons">
							<paper-button
									raised
									class="submit-button"
									on-tap="addPrice">
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
					<div id="add-Alt-tab">
						<div>
							<datalist id="alts">
								<template is="dom-repeat" items="[[alterations]]">
									<option value="{{item.name}}" />
								</template>
							</datalist>
							<paper-input id="addAltName"
									list="alts"
									value="{{altAddName}}"
									label="Name"
									on-value-changed="updateAltsDialog">
							</paper-input>
						</div>
						<div>
							<paper-input id="addAltDesc"
									name="description"
									value="{{altAddDesc}}"
									label="Description">
							</paper-input>
						</div>
						<div>
							<paper-input id="addAltStartDate"
									value="{{addProductStartDateAlt}}"
									name="addProductStartDateAlt"
									label="Start Date"
							</paper-input>
						</div>
						<div>
							<paper-input id="addAltEndDate"
									value="{{addProductEndDateAlt}}"
									name="addProductEndDateAlt"
									label="End Date"
							</paper-input>
						</div>
						<div>
							<paper-dropdown-menu
									label="Price Type"
									value="{{altAddType}}"
									on-selected-item-changed="checkRecureAlt">
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
						</div>
						<div>
							<paper-input
									id="addAltSize"
									value="{{altAddSize}}"
									name="size"
									label="Unit Size"
									type="text"
									allowed-pattern="[0-9kmg]"
									pattern="^[0-9]+[kmg]?$"
									auto-validate>
							</paper-input>
						</div>
						<div>
							<paper-dropdown-menu
									value="{{altAddUnit}}"
									label="Units">
								<paper-listbox
										id="addAltUnitDrop"
										on-selected-item-changed="checkPatternAlt"
										slot="dropdown-content">
									<paper-item id="altBytes">
											Bytes
									</paper-item>
									<paper-item id="altCents">
											Cents
									</paper-item>
									<paper-item id="altSeconds">
											Seconds
									</paper-item>
									<paper-item id="altMessages">
											Messages
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
						</div>
						<div>
							<paper-input id="addAltAmount"
									name="amount"
									label="Amount"
									type="text"
									allowed-pattern="[0-9.]"
									pattern="[0-9]+\.?[0-9]{0,6}$"
									auto-validate
									value=0>
							</paper-input>
						</div>
						<div>
							<paper-input
									id="addAltCurrency"
									name="currency"
									value="{{altAddCurrency}}"
									label="Currency">
							</paper-input>
						</div>
						<div>
							<paper-dropdown-menu
									id="addalt5drop"
									label="Period"
									value="{{altAddPeriod}}">
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
						</div>
						<div class="buttons">
							<paper-button
									raised
									class="submit-button"
									on-tap="addAlteration">
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
					id="addProductAjax"
					url="/catalogManagement/v2/productOffering"
					method = "POST"
					content-type="application/json"
					on-loading-changed="_onLoadingChanged"
					on-response="_addProductResponse"
					on-error="_addProductError">
			</iron-ajax>
			<iron-ajax
				id="getProductsAjax"
				url="/catalogManagement/v2/productOffering"
				method="GET"
				on-response="_getProductsResponse"
				on-error="_getProductsError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			listeners: {
				'checkIn.checked-changed': 'checkInChanged',
				'checkOut.checked-changed': 'checkOutChanged'
			}, 
			observers: [
				'_bundleCheckboxChanged(offers.*)'
			],
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
			offers: {
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
			addProductStartDateOffer: {
				type: String
			},
			addProductEndDateOffer: {
				type: String
			},
			addProductStartDatePrice: {
				type: String
			},
			addProductEndDatePrice: {
				type: String
			},
			addProductStartDateAlt: {
				type: String
			},
			addProductEndDateAlt: {
				type: String
			},
			addProductStartDateOfferPick: {
				observer: '_addProductStartDateOfferPick'
			},
			addProductEndDateOfferPick: {
				observer: '_addProductEndDateOfferPick'
			},
			addProductStartDatePricePick: {
				observer: '_addProductStartDatePricePick'
			},
			addProductEndDatePricePick: {
				observer: '_addProductEndDatePricePick'
			},
			addProductStartDateAltPick: {
				observer: '_addProductStartDateAltPick'
			},
			addProductEndDateAltPick: {
				observer: '_addProductEndDateAltPick'
			},
			offerAddAddress: {
				type: String
			},
			offerAddDes: {
				type: String
			},
			priceAddName: {
				type: String
			},
			priceAddDes: {
				type: String
			},
			priceAddSize: {
				type: String
			},
			priceAddCurrency: {
				type: String
			},
			priceAddTariff: {
				type: String
			},
			priceAddRoaming: {
				type: String
			},
			altAddName: {
				type: String
			},
			altAddDesc: {
				type: String
			},
			altAddSize: {
				type: String
			},
			altAddCurrency: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_getProductsResponse(event) {
		var results = event.detail.xhr.response;
		for (var index in results) {
			function checkExist(spec) {
				return spec.name == results[index].name;
			}
			if(!this.offers.some(checkExist)) {
				var product = new Object();
				product.id = results[index].id;
				product.href = results[index].href;
				product.name = results[index].name;
				product.checked = false;
				this.push('offers', product);
			}
		}
	}

	addUpdatePriceDialog() {
		function checkPriceUpdateName(price) {
			return price.name == this.priceAddName;
		}
		if(this.prices != undefined) {
			var indexPrice = this.prices.findIndex(checkPriceUpdateName);
			if (indexPrice == -1) {
				this.addOrUpdateButton = "add";
				this.priceAddDes = null;
				this.$.addPriceStartDate.value = null;
				this.$.addPriceEndDate.value = null;
				this.priceAddType = null;
				this.priceAddSize = null;
				this.priceAddUnits = null;
				this.$.addPriceAmount.value = null;
				this.priceAddCurrency = null;
				this.priceAddPeriod = null;
				this.priceAddAlter = null;
				this.$.addPriceCharReserveTime.value = null;
				this.$.addPriceCharReserveBytes.value = null;
				this.$.timeOfDayStart.value = null;
				this.$.timeOfDayEnd.value = null;
			} else {
				this.addOrUpdateButton = "update";
				this.priceAddDes = this.prices[indexPrice].description;
				this.$.addPriceStartDate.value = this.prices[indexPrice].start;
				this.$.addPriceEndDate.value = this.prices[indexPrice].end;
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
				this.priceAddSize = this.prices[indexPrice].size;
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
				this.priceAddCurrency = this.prices[indexPrice].currency;
				this.$.addPriceAmount.value = this.prices[indexPrice].amount;
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
				this.$.addPriceCharReserveTime.value = this.prices[indexPrice].reserveTime;
				this.$.addPriceCharReserveBytes.value = this.prices[indexPrice].reserveBytes;
				this.$.timeOfDayStart.value = this.prices[indexPrice].timeOfDayRange;
				this.$.timeOfDayEnd.value = this.prices[indexPrice].timeOfDayRange;
				this.priceCheckIn = this.prices[indexPrice].callDirection;
				this.priceCheckIn = this.prices[indexPrice].callDirection;
				this.priceAddTariff = this.prices[indexPrice].prefixTariff;
				this.priceAddRoaming = this.prices[indexPrice].roamingTable;
			}
		}
	}

	updateAltsDialog() {
		function checkName(alts) {
			return alts.name == this.altAddName;
		}
		if(this.alterations != undefined) {
			var index = this.alterations.findIndex(checkName);
			if (index == -1) {
				this.addOrUpdateButton = "add";
				this.altAddDesc = null;
				this.$.addAltStartDate.value = null;
				this.$.addAltEndDate.value = null;
				this.altAddType = null;
				this.altAddSize = null;
				this.altAddUnit = null;
				this.$.addAltAmount.value = null;
				this.altAddCurrency = null;
				this.altAddPeriod = null;
			} else {
				this.addOrUpdateButton = "update";
				this.altAddDesc = this.alterations[index].description;
				this.$.addAltStartDate.value = this.alterations[index].startdate;
				this.$.addAltEndDate.value = this.alterations[index].terminationDate;
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
				this.altAddSize = this.alterations[index].size;
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
				this.altAddCurrency = this.alterations[index].currency;
				this.$.addAltAmount.value = this.alterations[index].amount;
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
		if(this.offers.some(check)) {
			this.$.addOffProductSpecificationDrop.disabled = true;
			this.$.addOfferCharReserveSession.disabled = true;
		} else {
			this.$.addOffProductSpecificationDrop.disabled = false;
			this.$.addOfferCharReserveSession.disabled = false;
		}
	}

	_onClickBundle() {
		if(this.$.addBundle.opened == false) {
			this.$.addBundle.show();
			this.$.onClickBundle.icon="arrow-drop-up";
		} else {
			this.$.addBundle.hide();
			this.$.onClickBundle.icon="arrow-drop-down"
		}
	}

	_onClickOfferChars() {
		if(this.$.addOfferChars.opened == false) {
			this.$.addOfferChars.show();
			this.$.onClickOfferChars.icon="arrow-drop-up"
		} else {
			this.$.addOfferChars.hide();
			this.$.onClickOfferChars.icon="arrow-drop-down"
		}
	}

	addOffer() {
		var offerNew = new Object();
		if(this.offerAddAddress) {
			offerNew.name = this.offerAddAddress;
		}
		if(this.offerAddDes) {
			offerNew.description = this.offerAddDes;
		}
		if(this.$.addBundle) {
			var bundled = new Array();
			for(var index in this.offers) {
				if(this.offers[index].checked == true) {
					var bundleOffer = new Object();
					bundleOffer.numberRelOfferLowerLimit = 0;
					bundleOffer.numberRelOfferUpperLimit = 1;
					bundleOffer.numberRelOfferDefault = 1;
					var bundleObj = new Object();
					bundleObj.id = this.offers[index].id;
					bundleObj.href = this.offers[index].href;
					bundleObj.name = this.offers[index].name;
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
		if(this.addProductStartDateOffer) {
			startDateTime = this.addProductStartDateOffer;
		}
		if(this.addProductEndDateOffer) {
			endDateTime = this.addProductEndDateOffer;
		}
		if(startDateTime && endDateTime) {
			offerNew.validFor = {startDateTime, endDateTime};
		} else if(startDateTime && !endDateTime) {
			offerNew.validFor = {startDateTime};
		} else if(!startDateTime && endDateTime) {
			offerNew.validFor = {endDateTime};
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
				var currencyCode
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
			if (item.reserveTime) {
				var charValue = new Object();
				charValue.unitOfMeasure = "seconds";
				var charLength = item.reserveTime.length;
				var lastChar = item.reserveTime.charAt(charLength - 1); //gets last character
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
				var lastChar = item.reserveBytes.charAt(charLength - 1); //gets last character
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
				if (item.timeOfDayRange.length > 0) {
					var charValueUse = new Object();
					charValueUse.name = "timeOfDayRange";
					charValueUse.valueType = "Range";
					charValueUse.minCardinality = 0;
					charValueUse.maxCardinality = 1;
					var charValue1 = new Object();
					var charValue = new Object();
					var charValueLower = new Object();
					charValueLower.amount = this.addTimePickerStart.rawValue;
					charValueLower.units = "minutes";
					charValue.lowerValue = charValueLower;
					var charValueUpper = new Object();
					charValueUpper.amount = this.addTimePickerEnd.rawValue;
					charValueUpper.units = "minutes";
					charValue.upperValue = charValueUpper;
					charValue1.value = charValue;
					var charValues = new Array();
					charValues.push(charValue1);
					charValueUse.productSpecCharacteristicValue = charValues;
					prodSpecCharValueUse.push(charValueUse);
				}
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
			var ajax = this.$.addProductAjax;
			ajax.body = offerNew;
			ajax.generateRequest();
			this.$.addBundle.hide();
		} else {
			document.getElementById("addProductToastError").open();
		}
	}

	checkProductSpec() {
		if(this.offerAddSpec == "Prepaid Data") {
			this.$.destPrefixTariff.disabled = true;
		} else if(this.offerAddSpec == "Prepaid Voice") {
			this.$.destPrefixTariff.disabled = false;
		} else if(this.offerAddSpec == "Prepaid SMS") {
			this.$.destPrefixTariff.disabled = false;
		}
	}

	checkPattern() {
		if(this.priceAddUnits == "Bytes") {
			this.$.addPriceSize.allowedPattern = "[0-9kmg]";
			this.$.addPriceSize.pattern = "^[0-9]+[kmg]?$";
			this.$.addPriceSize.disabled = false;
			this.$.addPriceCharReserveBytes.allowedPattern = "[0-9kmg]";
			this.$.addPriceCharReserveBytes.pattern = "^[0-9]+[kmg]?$";
			this.$.addPriceCharReserveBytes.disabled = false;
			this.$.addPriceCharReserveTime.disabled = true;
		} else if(this.priceAddUnits == "Cents") {
			this.$.addPriceSize.allowedPattern = "[0-9]";
			this.$.addPriceSize.pattern = "^[0-9]+$";
			this.$.addPriceSize.disabled = true;
		} else if(this.priceAddUnits == "Seconds") {
			this.$.addPriceSize.allowedPattern = "[0-9mh]";
			this.$.addPriceSize.pattern = "^[0-9]+[mh]?$";
			this.$.addPriceSize.disabled = false;
			this.$.addPriceCharReserveTime.allowedPattern = "[0-9mh]";
			this.$.addPriceCharReserveTime.pattern = "^[0-9]+[mh]?$";
			this.$.addPriceCharReserveTime.disabled = false;
			this.$.addPriceCharReserveBytes.disabled = true;
		} else if(this.priceAddUnits == "Messages") {
			this.$.addPriceSize.allowedPattern = "[0-9]";
			this.$.addPriceSize.pattern = "^[0-9]+$";
			this.$.addPriceSize.disabled = false;
		}
	}

	checkPatternAlt() {
		if(this.altAddUnit == "Bytes") {
			this.$.addAltSize.allowedPattern = "[0-9kmg]";
			this.$.addAltSize.pattern = "^[0-9]+[kmg]?$";
			this.$.addAltSize.disabled= false;
		} else if(this.altAddUnit == "Cents") {
			this.$.addAltSize.allowedPattern = "[0-9]";
			this.$.addAltSize.pattern = "^[0-9]+$";
			this.$.addAltSize.disabled= true;
		} else if(this.altAddUnit == "Seconds") {
			this.$.addAltSize.allowedPattern = "[0-9mh]";
			this.$.addAltSize.pattern = "^[0-9]+[mh]?$";
			this.$.addAltSize.disabled = false;
		} else if(this.altAddUnit == "Messages") {
			this.$.addAltSize.allowedPattern = "[0-9]";
			this.$.addAltSize.pattern = "^[0-9]+$";
			this.$.addAltSize.disabled= false;
		}
	}

	checkRecure() {
		if(this.priceAddType == "Recurring") {
			this.$.addPricePerioddrop.disabled = false;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = true;
			this.$.priceMessages.disabled = true;
			this.$.priceCents.disabled = false;
			this.$.addPriceCharReserveTime.disabled = true;
			this.$.addPriceCharReserveBytes.disabled = true;
			this.$.addPriceUnits.selected = 1;
			this.$.addPriceAmount.false= false;
		} else if(this.priceAddType == "One Time") {
			this.$.addPricePerioddrop.disabled = true;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = true;
			this.$.priceMessages.disabled = true;
			this.$.priceCents.disabled = false;
			this.$.addPriceCharReserveTime.disabled = true;
			this.$.addPriceCharReserveBytes.disabled = true;
			this.$.addPriceUnits.selected = 1;
			this.$.addPriceAmount.disabled = false;
		} else if(this.priceAddType == "Usage") {
			this.$.addPricePerioddrop.disabled = true;
			this.$.priceCents.disabled = true;
			this.$.priceMessages.disabled = false;
			this.$.priceBytes.disabled = false;
			this.$.priceSeconds.disabled = false;
			this.$.addPriceUnits.selected = 0;
			this.$.addPriceAmount.disabled = false;
		} else if(this.priceAddType == "Tariff") {
			this.$.addPricePerioddrop.disabled = true;
			this.$.priceCents.disabled = true;
			this.$.priceBytes.disabled = true;
			this.$.priceMessages.disabled = false;
			this.$.priceSeconds.disabled = false;
			this.$.addPriceUnits.selected = 2;
			this.$.addPriceAmount.disabled = true;
			this.$.addPriceAmount.value = null;
		}
	}

	checkRecureAlt() {
		if(this.altAddType == "Recurring") {
			this.$.addalt5drop.disabled = false;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = false;
			this.$.altMessages.disabled = false;
			this.$.addAltUnitDrop.selected = 1;
		} else if(this.altAddType == "One Time") {
			this.$.addalt5drop.disabled = true;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = false;
			this.$.altMessages.disabled = false;
			this.$.addAltUnitDrop.selected = 1;
		} else if(this.altAddType == "Usage") {
			this.$.addalt5drop.disabled = true;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = true;
			this.$.altMessages.disabled = false;
		}
	}
	addPrice() {
		function checkPriceName(price) {
			return price.name == this.$.addPriceName.value;
		}
		var indexPrice = this.prices.findIndex(checkPriceName);
		if (indexPrice == -1) {
			var priceNew = new Object();
		} else {
			var priceNew = this.prices[indexPrice];
		}
		priceNew.name = this.priceAddName;
		priceNew.description = this.priceAddDes;
		priceNew.start = this.addProductStartDatePrice;
		priceNew.end = this.addProductEndDatePrice;
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
		if (this.$.addPriceAmount.value) {
			priceNew.amount = this.$.addPriceAmount.value;
		}
		priceNew.size = this.priceAddSize;
		priceNew.currency = this.priceAddCurrency;
		switch(this.$.addPricePeriod.selected) {
			case 0:
				priceNew.period = "hourly";
			break
			case 1:
				priceNew.period = "daily";
			break;
			case 2:
				priceNew.period = "weekly";
			break;
			case 3:
				priceNew.period = "monthly";
			break
			case 4:
				priceNew.period = "yearly";
			break;
		}
		if(this.priceAddAlter) {
			function checkAlt(alts) {
				return alts.name == this.priceAddAlter;
			}
			var altIndex = this.alterations.findIndex(checkAlt);
			priceNew.alterations = this.alterations[altIndex];
		}
		priceNew.reserveTime = this.$.addPriceCharReserveTime.value;
		priceNew.reserveBytes = this.$.addPriceCharReserveBytes.value;
		priceNew.timeOfDayRange = this.$.timeOfDayStart.value;
		priceNew.timeOfDayRange = this.$.timeOfDayEnd.value;
		if(this.$.checkIn.checked == true) {
			priceNew.callDirection = "answer";
		} else if(this.$.checkOut.checked == true) {
			priceNew.callDirection = "originate";
		}
		priceNew.prefixTariff = this.priceAddTariff;
		priceNew.roamingTable = this.priceAddRoaming;
		if(priceNew.name
					&& priceNew.type
					&& priceNew.unit
					&& (priceNew.size || priceNew.unit == "c")
					&& (priceNew.amount || priceNew.type == "tariff")) {
			if (indexPrice == -1) {
				this.push('prices', priceNew);
			} else {
				this.splice('prices', indexPrice, 1, priceNew);
				this.addOrUpdateButton = "add";
			}
			this.priceAddName = null;
			this.priceAddDes = null;
			this.$.addPriceStartDate.value = null;
			this.$.addPriceEndDate.value = null;
			this.priceAddType = null;
			this.priceAddSize = null;
			this.$.addPriceUnits.selected = null;
			this.$.addPriceAmount.value = null;
			this.$.addPriceCurrency.value = null;
			this.$.addPricePeriod.selected = null;
			this.$.addPriceAlteration.selected = null;
			this.$.addPriceCharReserveTime.value = null;
			this.$.addPriceCharReserveBytes.value = null;
			this.$.destPrefixTariff.value = null;
			this.$.roamingTable.value = null;
			this.$.timeOfDayStart.value = null;
			this.$.timeOfDayEnd.value = null;
//			document.getElementById("addProductPriceToastSuccess").open();
		} else {
			document.getElementById("addProductToastError").open();
		}
	}

	_onClickPriceChars() {
		if(this.$.addPriceChars.opened == false) {
			this.$.addPriceChars.show();
			this.$.onClickPriceChars.icon="arrow-drop-up"
		} else {
			this.$.addPriceChars.hide();
			this.$.onClickPriceChars.icon="arrow-drop-down"
		}
	}

	_onClickPriceCharsTime() {
		if(this.$.addPriceCharsTime.opened == false) {
			this.$.addPriceCharsTime.show();
			this.$.onClickPriceCharsTime.icon="arrow-drop-up"
		} else {
			this.$.addPriceCharsTime.hide();
			this.$.onClickPriceChars.icon="arrow-drop-down"
		}
	}

	_onClickCall() {
		if(this.$.addCall.opened == false) {
			this.$.addCall.show();
			this.$.onClickCall.icon="arrow-drop-up";
		} else {
			this.$.addCall.hide();
			this.$.onClickCall.icon="arrow-drop-down"
		}
	}

	checkInChanged(event) {
		if(event.detail.value) {
			this.$.checkOut.checked = false;
		}
	}

	checkOutChanged(event) {
		if(event.detail.value) {
			this.$.checkIn.checked = false;
		}
	}

	addAlteration() {
		function checkAltName(alt) {
			return alt.name == this.altAddName;
		}
		var indexAlt = this.alterations.findIndex(checkAltName);
		if (indexAlt == -1) {
			var altNew = new Object();
		} else {
			var altNew = this.prices[indexAlt];
		}
		var altNew = new Object();
		altNew.name = this.altAddName;
		altNew.description = this.altAddDesc;
		altNew.startdate = this.addProductStartDateAlt;
		altNew.terminationDate = this.addProductEndDateAlt;
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
		altNew.size = this.altAddSize;
		if(this.$.addAltAmount.value) {
			altNew.amount= this.$.addAltAmount.value;
		}
		altNew.currency = this.$.addAltCurrency.value;
		switch(this.$.addAltPeriod.selected) {
			case 0:
				altNew.period = "hourly";
			break
			case 1:
				altNew.period = "daily";
			break;
			case 2:
				altNew.period = "weekly";
			break;
			case 3:
				altNew.period = "monthly";
			break
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
			break
			case 2:
				altNew.unit = "s";
			break
			case 3:
				altNew.unit = "msg";
			break
		}
		if(altNew.name
					&& altNew.type
					&& altNew.unit
					&& (altNew.size || altNew.unit == "c")
					&& (altNew.amount || altNew.amount == 0)) {
			if (indexAlt == -1) {
				this.push('alterations', altNew);
			} else {
				this.splice('alterations', indexAlt, 1, altNew);
				this.addOrUpdateButton = "add";
			}
			this.altAddName = null
			this.altAddDesc = null;
			this.$.addAltStartDate.value = null;
			this.$.addAltEndDate.value = null;
			this.$.addAltType.selected = null;
			this.altAddSize = null;
			this.$.addAltUnitDrop.selected = null;
			this.$.addAltAmount.value = null;
			this.$.addAltCurrency.value = null;
			this.$.addAltPeriod.selected = null;
//			document.getElementById("addProductAlterationToastSuccess").open();
		} else {
			document.getElementById("addProductToastError").open();
		}
	}

	_addProductResponse() {
		this.$.addProductModal.close();
//		document.getElementById("addProductToastSuccess").open();
		this.offerAddAddress = null;
		this.offerAddDes = null;
		this.set('prices', []);
		this.set('alterations', []);
		document.body.querySelector('sig-app').shadowRoot.getElementById('offerList').shadowRoot.getElementById('offerGrid').clearCache();
		this.cancelDialog();
	}

	cancelDialog() {
		this.set('prices', []);
		this.set('alterations', []);
		this.set('offers', []);
		this.addOrUpdateButton = "add";
		this.offerAddAddress = null;
		this.offerAddDes = null;
		this.$.addOffProductSpecification.selected = null;
		this.$.addOffStart.value = null;
		this.$.addOffEnd.value = null;
		this.$.addOfferChars.hide();
		this.$.addBundle.hide();
		this.$.destPrefixTariff.value = null;
		this.$.roamingTable.value = null;
		this.$.addOfferCharReserveSession.value = null;
		this.addProductStartDateOffer = null;
		this.addProductEndDateOffer = null;
		this.$.addPriceName.value = null;
		this.$.addPriceDesc.value = null;
		this.$.addPriceStartDate.value = null;
		this.$.addPriceEndDate.value = null;
		this.$.addPriceType.selected = null;
		this.$.addPriceSize.value = null;
		this.$.addPriceUnits.selected = null;
		this.$.addPriceAmount.value = null;
		this.$.addPriceCurrency.value = null;
		this.$.addPricePeriod.selected = null;
		this.$.addPriceCharReserveTime.value = null;
		this.$.addPriceCharReserveBytes.value = null;
		this.altAddName = null
		this.altAddDesc = null;
		this.$.addAltStartDate.value = null;
		this.$.addAltEndDate.value = null;
		this.$.addAltType.selected = null;
		this.altAddSize = null;
		this.$.addAltUnitDrop.selected = null;
		this.$.addAltAmount.value = null;
		this.$.addAltCurrency.value = null;
		this.$.addAltPeriod.selected = null;
		this.$.timeOfDayStart.value = null;
		this.$.timeOfDayEnd.value = null;
		this.$.addProductModal.close();
	}
}

window.customElements.define('sig-offer-add', offerAdd);
