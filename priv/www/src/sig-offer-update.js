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
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js';
import '@polymer/paper-checkbox/paper-checkbox.js';
import '@polymer/iron-collapse/iron-collapse.js';
import '@polymer/paper-tabs/paper-tabs.js';
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/paper-icon-button/paper-icon-button.js';
import '@polymer/paper-tooltip/paper-tooltip.js';
import './style-element.js';

class offerUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updateOfferModal" modal>
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
						id="progressId"
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<iron-pages
						selected="{{selected}}">
					<div id="addOff-tab">
						<paper-input
								id="updateOffName"
								value="{{updateOfferName}}"
								label="Name"
								disabled>
						</paper-input>
						<paper-input
								id="updateOffDesc"
								value="{{updateOfferDesc}}"
								label="Description">
						</paper-input>
						<paper-tooltip
								for="updateOffDesc"
								offset="0">
							Add a value to update the offer description
						</paper-tooltip>
						<div>
							<span>Bundled Products</span>
							<paper-icon-button
								id="onClickBundleUpdate"
								suffix
								icon="arrow-drop-down"
								on-click="_onClickBundleUpdate">
							</paper-icon-button>
						</div>
						<iron-collapse id="addBundleUpdate">
							<template is=dom-repeat items="{{offers}}">
								<div>
									<paper-checkbox class="bundleCheck" checked="{{item.checked}}">
										{{item.name}}
									</paper-checkbox>
								</div>
							</template>
						</iron-collapse>
						<paper-input id="updateOffSpec"
								value="{{updateOfferSpec}}"
								label="Product Specification"
								disabled>
						</paper-input>
						<paper-tooltip
								for="updateOffSpec"
								offset="0">
							Add a value to update the offer specification
						</paper-tooltip>
						<paper-input id="updateOffStart"
								value="{{updateOfferStartDate}}"
								label="Start Date">
						</paper-input>
						<paper-tooltip
								for="updateOffStart"
								offset="0">
							Add a value to update the offer start date
						</paper-tooltip>
						<paper-input id="updateOffEnd"
								value="{{updateOfferEndDate}}"
								label="End Date">
						</paper-input>
						<paper-tooltip
								for="updateOffEnd"
								offset="0">
							Add a value to update the offer end date
						</paper-tooltip>
						<div>
							<span>Characteristics</span>
							<paper-icon-button
									id="updateOnClickOfferChars"
									suffix
									icon="arrow-drop-down"
									on-click="_updateOnClickOfferChars">
							</paper-icon-button>
						</div>
						<iron-collapse id="updateAddPriceOfferChars">
							<paper-input
									id="updateReserveSession"
									allowed-pattern="[0-9mh]"
									pattern="^[0-9]+[mh]?$"
									auto-validate
									label="RADIUS Reserve Session"
									value=0>
							</paper-input>
							<paper-tooltip
									for="updateReserveSession"
									offset="0">
								Add a value to update the offer reserve session
							</paper-tooltip>
							<paper-input
									id="updateRedirectServer"
									allowed-pattern="[0-9\.]"
									label="Redirect Server"
									value="{{updateRedirect}}">
							</paper-input>
							<paper-tooltip
									for="updateRedirectServer"
									offset="0">
								Add a value to update the offer reserve session
							</paper-tooltip>
						</iron-collapse>
						<div class="buttons">
							<paper-button
									autofocus
									on-tap="_updateProductOffer"
									class="update-button">
								Update
							</paper-button>
							<paper-button
									dialog-dismiss
									class="cancel-button"
									on-tap="cancelDialog">
								cancel
							</paper-button>
							<paper-button
									toggles
									raised
									on-tap="_deleteOffer"
									class="delete-button">
								Delete
							</paper-button>
						</div>
					</div>
					<div id=updatePrice-tab>
						<datalist id="updatePriceNames">
							<template is="dom-repeat" items="{{prices}}">
								<option value="{{item.name}}" />
							</template>
						</datalist>
						<input id="updatePriceName" 
								list="updatePriceNames"
								value="{{priceUpdateName::input}}"
								placeholder="Name"/>
						<paper-tooltip
								for="updatePriceName"
								offset="0">
							Add a value to update the offer price name
						</paper-tooltip>
						<paper-input id="updatePriceDesc"
								value="{{priceUpdateDesc}}"
								label="Description">
						</paper-input>
						<paper-tooltip
								for="updatePriceDesc"
								offset="0">
							Add a value to update the offer price description
						</paper-tooltip>
						<paper-input id="updatePriceStartDate"
								value="{{updateOfferStartDatePrice}}"
								label="Start Date">
						</paper-input>
						<paper-tooltip
								for="updatePriceStartDate"
								offset="0">
							Add a value to update the offer price start date
						</paper-tooltip>
						<paper-input id="updatePriceEndDate"
								value="{{updateOfferEndDatePrice}}"
								label="End Date">
						</paper-input>
						<paper-tooltip
								for="updatePriceEndDate"
								offset="0">
							Add a value to update the offer price end date
						</paper-tooltip>
						<paper-dropdown-menu id="updatePriceTypedrop"
								label="Price Type"
								value="{{priceUpdateType}}"
								no-animations="true"
								on-selected-item-changed="checkRecure">
							<paper-listbox
									id="updatePriceType"
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
						<paper-tooltip
								for="updatePriceTypedrop"
								offset="0">
							Select a value to update the offer price type (Recurring | Onetime | Usage | Tariff)
						</paper-tooltip>
						<paper-input id="updatePriceSize"
								value="{{priceUpdateSize}}"
								allowed-pattern="[0-9kmg]"
								pattern="^[0-9]+[kmg]?$"
								label="Unit Size"
								auto-validate>
						</paper-input>
						<paper-tooltip
								for="updatePriceSize"
								offset="0">
							Unit of measure (Bytes=(MB="m", GB="g", KB="k"), Seconds=(Minutes="m",Hour="h"), Messages=(Messages="msg"))
						</paper-tooltip>
						<paper-dropdown-menu id="updatePriceUnitsdrop"
								label="Units"
								value="{{priceUpdateUnits}}"
								no-animations="true"
								on-selected-item-changed="checkPattern">
							<paper-listbox
									id="updatePriceUnits"
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
							</paper-listbox>
						</paper-dropdown-menu>
						<paper-tooltip
								for="updatePriceUnitsdrop"
								offset="0">
							Select a value to update the offer price unit (Bytes | Cents | Seconds)
						</paper-tooltip>
						<paper-input id="updatePriceAmount"
								value="{{priceUpdateAmount}}"
								type="text"
								allowed-pattern="[0-9.]"
								pattern="[0-9]+\.?[0-9]{0,6}$"
								auto-validate
								label="Amount"
								value=0>
						</paper-input>
						<paper-tooltip
								for="updatePriceAmount"
								offset="0">
							Add a value to update the offer price tax included amount
						</paper-tooltip>
						<paper-input id="updatePriceCurrency"
								value="{{priceUpdateCurrency}}"
								label="Currency">
						</paper-input>
						<paper-tooltip
								for="updatePriceCurrency"
								offset="0">
							Add a value to update the offer price tax currency
						</paper-tooltip>
						<paper-dropdown-menu id="updatePricePerioddrop"
								value="{{priceUpdatePeriod}}"
								no-animations="true"
								label="Period">
							<paper-listbox
									id="updatePricePeriod"
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
						<paper-tooltip
								for="updatePricePeriod"
								offset="0">
							Add a value to update the offer price period (Hour | Daily | Weekly | Monthly | Yearly)
						</paper-tooltip>
						<div>
							<paper-dropdown-menu
									id="addUpdatePriceDrop"
									value="{{priceUpdateAlt}}"
									no-animations="true"
									label="Alterations">
								<paper-listbox
										id="addUpdatePriceAlteration"
										slot="dropdown-content">
									<template is="dom-repeat" items="[[alterations]]">
										<paper-item>
											{{item.name}}
										</paper-item>
									</template>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip
									for="addUpdatePriceDrop"
									offset="0">
								Add a value to update the offer price alteration
							</paper-tooltip>
						</div>
						<div>
							<span>Characteristics</span>
							<paper-icon-button
									id="updateOnClickChars"
									suffix
									icon="arrow-drop-down"
									on-click="_updateOnClickChars">
							</paper-icon-button>
						</div>
						<iron-collapse id="updateAddPriceChars">
							<div>
								<span>Time of day range</span>
								<paper-icon-button
										id="updateOnClickCharsTime"
										suffix
										icon="arrow-drop-down"
										on-click="_updateOnClickCharsTime">
								</paper-icon-button>
							</div>
							<iron-collapse id="updatePriceCharsTime">
								<paper-input
										id="updateTimeOfDayStart"
										value="{{startTimeUpdate}}"
										label="Start Time">
								</paper-input>
								<paper-tooltip
										for="updateTimeOfDayStart"
										offset="0">
									Add a value to update the offer price start time of day
								</paper-tooltip>
								<paper-input
										id="updateTimeOfDayEnd"
										value="{{endTimeUpdate}}"
										label="End Time">
								</paper-input>
								<paper-tooltip
										for="updateTimeOfDayEnd"
										offset="0">
									Add a value to update the offer price end time of day
								</paper-tooltip>
							</iron-collapse>
							<div>
								<span>Call Direction</span>
								<paper-icon-button
										id="updateOnClickCall"
										suffix
										icon="arrow-drop-down"
										on-click="_updateOnClickCall">
								</paper-icon-button>
							</div>
							<iron-collapse id="updateAddCall">
								<paper-checkbox
										id="updateCheckIn"
										value="{{priceUpdateCheckIn}}">
									Incoming
								</paper-checkbox>
								<paper-checkbox
										id="updateCheckOut"
										value="{{priceUpdateCheckOut}}">
									Outgoing
								</paper-checkbox>
							</iron-collapse>
							<paper-input
									id="updateAddPriceCharReserveTime"
									type="number"
									label="RADIUS Reserve Time"
									value=0>
							</paper-input>
							<paper-tooltip
									for="updateAddPriceCharReserveTime"
									offset="0">
								Add a value to update the offer price characteristic reserve time
							</paper-tooltip>
							<paper-input
									id="updateAddPriceCharReserveBytes"
									type="number"
									label="RADIUS Reserve Data"
									value=0>
							</paper-input>
							<paper-tooltip
									for="updateAddPriceCharReserveBytes"
									offset="0">
								Add a value to update the offer price characteristic reserve bytes
							</paper-tooltip>
							<paper-input
									id="updateDestPrefixTariff"
									value="{{priceUpdateTariff}}"
									type="string"
									label="Prefix Tariff Table">
							</paper-input>
							<paper-tooltip
									for="updateDestPrefixTariff"
									offset="0">
								Add a value to update the offer price destination prefix tariff
							</paper-tooltip>
							<paper-input
									id="roamingTable"
									type="string"
									value="{{priceAddRoaming}}"
									label="Roaming Table">
							</paper-input>
							<paper-input
									id="chargingKey"
									value="{{chargingKey}}"
									type="number"
									label="Charging Key">
							</paper-input>
						</iron-collapse>
						<div class="buttons">
							<paper-button
									autofocus
									id="updateOfferPriceButton"
									on-tap="_updateOfferPrice"
									class="update-button"
									hidden>
								Update
							</paper-button>
							<paper-button
									raised
									id="updateOfferAddButton"
									class="submit-button"
									on-tap="updateAddPrice">
								Add
							</paper-button>
							<paper-button
									dialog-dismiss
									class="cancel-button"
									on-tap="cancelDialog">
								Cancel
							</paper-button>
						</div>
					</div>
					<div id="add-Alt-tab">
						<div>
							<datalist id="updateAltNames">
								<template is="dom-repeat" items="{{alterations}}">
									<option value="{{item.name}}"/>
								</template>
							</datalist>
							<input id="updateAltName" 
									list="updateAltNames" 
									value="{{AltUpdateName::input}}"
									placeholder="Name"/>
							<paper-tooltip
									for="updateAltName"
									offset="0">
								Add a value to update the offer price alteration name
							</paper-tooltip>
						</div>
						<paper-input id="updateAltDesc"
								value="{{AltUpdateDesc}}"
								label="Description">
						</paper-input>
						<paper-tooltip
								for="updateAltDesc"
								offset="0">
							Add a value to update the offer price alteration description
						</paper-tooltip>
						<paper-input id="updateAltStartDate"
								value="{{updateOfferStartDateAlt}}"
								label="Start Date">
						</paper-input>
						<paper-tooltip
								for="updateAltStartDate"
								offset="0">
							Add a value to update the offer price alteration start date
						</paper-tooltip>
						<paper-input id="updateAltEndDate"
								value="{{updateOfferEndDateAlt}}"
								label="End Date">
						</paper-input>
						<paper-tooltip
								for="updateAltEndDate"
								offset="0">
							Add a value to update the offer price alteration end date
						</paper-tooltip>
						<paper-dropdown-menu
								id="updateAltTypedrop"
								label="Price Type"
								value="{{altUpdateType}}"
								no-animations="true"
								on-selected-item-changed="checkRecureAlt">
							<paper-listbox
									id="updateAltType"
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
						<paper-tooltip
								for="updateAltTypedrop"
								offset="0">
							Add a value to update the offer price alteration type (Recurring | Onetime | Usage)
						</paper-tooltip>
						<paper-input id="updateAltSize"
								label="Unit Size"
								type="text"
								allowed-pattern="[0-9kmg]"
								pattern="^[0-9]+[kmg]?$"
								auto-validate
								value=1>
						</paper-input>
						<paper-tooltip
								for="updateAltSize"
								offset="0">
							Unit of measure (Bytes=(MB="m", GB="g", KB="k"), Seconds=(Minutes="m",Hour="h"), Messages=(Messages="msg"))
						</paper-tooltip>
						<paper-dropdown-menu
								id="updateAltsUnitsdrop"
								value= "{{altUpdateUnits}}"
								no-animations="true"
								label="Units">
							<paper-listbox
									id="updateUnitDrop"
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
							</paper-listbox>
						</paper-dropdown-menu>
						<paper-tooltip
								for="updateAltsUnitsdrop"
								offset="0">
							Select a value to update the offer price alteration unit (Bytes | Cents | Seconds)
						</paper-tooltip>
						<paper-input id="updateAltAmount"
								label="Amount"
								type="text"
								allowed-pattern="[0-9.]"
								pattern="[0-9]+\.?[0-9]{0,6}$"
								auto-validate
								value=0>
						</paper-input>
						<paper-tooltip
								for="updateAltAmount"
								offset="0">
							Add a value to update the offer price alteration tax included amount
						</paper-tooltip>
						<paper-input id="updateAltCurr"
								value="{{altUpdateCurr}}"
								label="Currency">
						</paper-input>
						<paper-tooltip
								for="updateAltCurr"
								offset="0">
							Add a value to update the offer price alteration currency
						</paper-tooltip>
						<paper-dropdown-menu id="addalt5drop"
								label="Period"
								no-animations="true"
								value="{{AltUpdatePer}}">
							<paper-listbox
									id="updateAltPeriod"
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
						<paper-tooltip
								for="addalt5drop"
								offset="0">
							Add a value to update the offer price alteration period (Hour | Daily | Weekly | Monthly | Yearly)
						</paper-tooltip>
						<div class="buttons">
							<paper-button
									id="updateOfferAlterationButton"
									autofocus
									on-tap="_updateOfferAlteration"
									class="update-button"
									hidden>
								Update
							</paper-button>
							<paper-button
									raised
									id="updateAddAlterationButton"
									class="submit-button"
									on-tap="updateAddAlteration">
								Add
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
					id="updateProductOfferAjax"
					method="PATCH"
					content-type="application/json-patch+json"
					on-loading-changed="_onLoadingChanged"
					on-response="_updateProductOfferResponse"
					on-error="_updateProductOfferError">
			</iron-ajax>
			<iron-ajax
					id="updateOfferPriceAjax"
					on-response="_updateOfferPriceResponse"
					on-error="_updateOfferPriceError">
			</iron-ajax>
			<iron-ajax
					id="deleteOfferAjax"
					method="DELETE"
					on-response="_deleteOfferResponse"
					on-error="_deleteOfferError">
			</iron-ajax>
			<iron-ajax
					id="updateOfferAlterationAjax"
					on-response="_updateOfferAlterationResponse"
					on-error="_updateOfferAlterationError">
			</iron-ajax>
			<iron-ajax
					id="getOffersAjax"
					url="/catalogManagement/v2/productOffering"
					on-response="_getProductUpdateResponse"
					on-error="_getProductsError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			listeners: {
				'updateCheckIn.checked-changed': 'updateCheckInChanged',
				'updateCheckOut.checked-changed': 'updateCheckOutChanged',
			},
			activeItem: {
				type: Object,
				observer: '_activeItemChanged'
			},
			priceUpdateName: {
				observer: 'updatePriceDialog'
			},
			AltUpdateName: {
				observer: 'updateAddAltsDialog'
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
			characteristics: {
				type: Array,
			},
			loading: {
				type: Boolean,
				value: false
			},
			selected: {
				type: Number,
				value: 0
			},
			updateOfferName: {
				type: String
			},
			updateOfferDesc: {
				type: String
			},
			updateOfferSpec: {
				type: String
			},
			updateOfferStartDate: {
				type: String
			},
			updateRedirect: {
				type: String
			},
			updateOfferEndDate: {
				type: String
			},
			priceUpdateDesc: {
				type: String
			},
			chargingKey: {
				type: Number
			},
			updateOfferStartDatePrice: {
				type: String
			},
			updateOfferEndDatePrice: {
				type: String
			},
			priceUpdateSize: {
				type: String
			},
			priceUpdateCurrency: {
				type: String
			},
			startTimeUpdate: {
				type: String
			},
			endTimeUpdate: {
				type: String
			},
			priceUpdateTariff: {
				type: String
			},
			AltUpdateDesc: {
				type: String
			},
			updateOfferStartDateAlt: {
				type: String
			},
			updateOfferEndDateAlt: {
				type: String
			},
			altUpdateCurr: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item, last) {
		if(item || last) {
			var current;
			if(item) {
				current = item;
			} else if(last) {
				current = last;
			}
			this.$.getOffersAjax.generateRequest();
			this.$.updateOfferModal.open();
			this.updateOfferName = current.id;
			this.updateOfferDesc = current.description;
			if(current.productSpecification && current.productSpecification.name) {
				this.updateOfferSpec = current.productSpecification.name.replace("ProductSpec", "");
			}
			this.updateOfferStartDate = current.startDate;
			this.updateOfferEndDate = current.endDate;
			this.characteristics = current.prodSpecCharValueUse;
			for (var indexCha in current.prodSpecCharValueUse) {
				if(current.prodSpecCharValueUse[indexCha].name == "radiusReserveSessionTime") {
					this.$.updateReserveSession.value = current.prodSpecCharValueUse[indexCha].productSpecCharacteristicValue[0].value;
				}
				if(current.prodSpecCharValueUse[indexCha].name == "redirectServer") {
					this.updateRedirect = current.prodSpecCharValueUse[indexCha].productSpecCharacteristicValue[0].value;
				}
			}
			for(var index in current.prices) {
				var newPrice = new Object();
				newPrice.name = current.prices[index].name;
				newPrice.description = current.prices[index].description;
				if(current.prices[index].validFor) {
					newPrice.start = current.prices[index].validFor.startDateTime;
					newPrice.end = current.prices[index].validFor.endDateTime;
				}
				switch(current.prices[index].priceType) {
					case "recurring":
						newPrice.priceType = "Recurring";
						break;
					case "one_time":
						newPrice.priceType = "One Time";
						break;
					case "usage":
						newPrice.priceType = "Usage";
						break;
					case "tariff":
						newPrice.priceType = "Tariff";
						break;
				}
				if (current.prices[index].unitOfMeasure) {
					var unitOfMeasure = current.prices[index].unitOfMeasure;
					switch(unitOfMeasure.charAt(unitOfMeasure.length - 1)) {
						case "b":
							var conv;
							conv = unitOfMeasure = parseInt(unitOfMeasure.slice(0, -1));
							newPrice.size = conv.toString();
							newPrice.unit = "b";
							break;
						case "s":
							var conv1;
							conv1 = parseInt(unitOfMeasure.slice(0, -1));
							newPrice.size = conv1.toString();
							newPrice.unit = "s";
							break;
					}
				}
				if(current.prices[index].price) {
					newPrice.currency = current.prices[index].price.currencyCode;
					newPrice.amount = current.prices[index].price.taxIncludedAmount;
				}
				var prodPrice = current.prices[index];
				if(prodPrice.prodSpecCharValueUse) {
					var specChar = new Array();
					for (var indexChar in prodPrice.prodSpecCharValueUse) {
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "radiusReserveTime") {
							specChar[indexChar] = {name: "radiusReserveTime", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "radiusReserveOctets") {
							specChar[indexChar] = {name: "radiusReserveOctets", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "timeOfDayRange") {
							specChar[indexChar] = {name: "timeOfDayRange", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "callDirection") {
							specChar[indexChar] = {name: "callDirection", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "destPrefixTariffTable") {
							specChar[indexChar] = {name: "destPrefixTariffTable", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "roamingTable") {
							specChar[indexChar] = {name: "roamingTable", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
						if(prodPrice.prodSpecCharValueUse[indexChar].name == "chargingKey") {
							specChar[indexChar] = {name: "chargingKey", value: prodPrice.prodSpecCharValueUse[indexChar].productSpecCharacteristicValue[0].value};
						}
					}
				}
				newPrice.prodSpecCharValueUse = specChar;
				switch(current.prices[index].recurringChargePeriod) {
					case "hourly":
						newPrice.period = "Hourly";
						break;
					case "daily":
						newPrice.period = "Daily";
						break;
					case "weekly":
						newPrice.period = "Weekly";
						break;
					case "monthly":
						newPrice.period = "Monthly";
						break;
					case "yearly":
						newPrice.period = "Yearly";
						break;
				}

				if(current.prices[index].productOfferPriceAlteration) {
					var altName = current.prices[index].productOfferPriceAlteration.name;
					function checkAlt1(alt) {
						return alt.name == altName;
					}
					var altIndex = this.alterations.findIndex(checkAlt1);
					if(altIndex == -1) {
						var newAlt = new Object();
						newAlt.name = current.prices[index].productOfferPriceAlteration.name;
						newAlt.description = current.prices[index].productOfferPriceAlteration.description;
						if(current.prices[index].validFor) {
							newAlt.start = current.prices[index].productOfferPriceAlteration.validFor.startDateTime;
							newAlt.end = current.prices[index].productOfferPriceAlteration.validFor.endDateTime;
						}
						newAlt.priceType = current.prices[index].productOfferPriceAlteration.priceType;
						if (current.prices[index].productOfferPriceAlteration.unitOfMeasure) {
							var unitOfMeasure = current.prices[index].productOfferPriceAlteration.unitOfMeasure;
							switch(unitOfMeasure.charAt(unitOfMeasure.length - 1)) {
								case "b":
									newAlt.size = parseInt(unitOfMeasure.slice(0, -1));
									newAlt.unit = "b";
									break;
								case "s":
									newAlt.size = parseInt(unitOfMeasure.slice(0, -1));
									newAlt.unit = "s";
									break;
								default:
									newAlt.unit = "c";
							}
						}
						if(current.prices[index].productOfferPriceAlteration.price) {
							newAlt.currency = current.prices[index].productOfferPriceAlteration.price.currencyCode;
							newAlt.amount = current.prices[index].productOfferPriceAlteration.price.taxIncludedAmount;
						}
						newAlt.period = current.prices[index].productOfferPriceAlteration.recurringChargePeriod;
						this.push('alterations', newAlt);
						newPrice.alteration = newAlt.name;
					}
				}
				function checkExist(price) {
					return price.name == current.prices[index].name;
				}
				if(!this.prices.some(checkExist)) {
					this.push('prices', newPrice);
				}
			}
		}
	}

	_getProductUpdateResponse(event) {
		var results = event.detail.xhr.response;
		function checkExist(spec) {
			return spec.name == results[index].name;
		}
		for (var index in results) {
			if(!this.offers.some(checkExist)) {
				var offer = new Object();
				offer.id = results[index].id;
				offer.href = results[index].href;
				offer.name = results[index].name;
				offer.checked = false;
				this.push('offers', offer);
			}
		}
		var bundle = document.body.querySelector('sig-app').shadowRoot.getElementById('offerList').shadowRoot.getElementById('offerGrid').activeItem.bundledProductOffering;
		for(var indexBun in bundle) {
			function checkExist1(prod) {
				return prod.name == bundle[indexBun].name;
			}
			var ind = this.offers.findIndex(checkExist1);
			if (ind != -1) {
				this.offers[ind].checked = true;
			}
		}
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
	}

	_getProductsError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_updateOnClickOfferChars() {
		if(this.$.updateAddPriceOfferChars.opened == false) {
			this.$.updateAddPriceOfferChars.show();
			this.$.updateOnClickOfferChars.icon="arrow-drop-up"
		} else {
			this.$.updateAddPriceOfferChars.hide();
			this.$.updateOnClickOfferChars.icon="arrow-drop-down"
		}
	}

	_updateOnClickChars() {
		if(this.$.updateAddPriceChars.opened == false) {
			this.$.updateAddPriceChars.show();
			this.$.updateOnClickChars.icon="arrow-drop-up"
		} else {
			this.$.updateAddPriceChars.hide();
			this.$.updateOnClickChars.icon="arrow-drop-down"
		}
	}

	_updateOnClickCharsTime() {
		if(this.$.updatePriceCharsTime.opened == false) {
			this.$.updatePriceCharsTime.show();
			this.$.updateOnClickCharsTime.icon="arrow-drop-up"
		} else {
			this.$.updatePriceCharsTime.hide();
			this.$.updateOnClickCharsTime.icon="arrow-drop-down"
		}
	}

	_updateOnClickCall() {
		if(this.$.updateAddCall.opened == false) {
			this.$.updateAddCall.show();
			this.$.updateOnClickCall.icon="arrow-drop-up"
		} else {
			this.$.updateAddCall.hide();
			this.$.updateOnClickCall.icon="arrow-drop-down"
		}
	}

	updateCheckInChanged(event) {
		if(event.detail.value) {
			this.$.updateCheckOut.checked = false;
		}
	}

	updateCheckOutChanged(event) {
		if(event.detail.value) {
			this.$.updateCheckIn.checked = false;
		}
	}

	_onClickBundleUpdate() {
		if(this.$.addBundleUpdate.opened == false) {
			this.$.addBundleUpdate.show();
			this.$.onClickBundleUpdate.icon="arrow-drop-up";
		} else {
			this.$.addBundleUpdate.hide();
			this.$.onClickBundleUpdate.icon="arrow-drop-down";
		}
	}

	updatePriceDialog() {
		function checkUpdatePriceName(updatePrice) {
			return updatePrice.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updatePriceName').value;
		}
		if(this.prices != undefined) {
			var indexUpdatePrice = this.prices.findIndex(checkUpdatePriceName);
			if(indexUpdatePrice != -1) {
				this.$.updateOfferAddButton.hidden = true;
				this.$.updateOfferPriceButton.hidden = false;
			} else {
				this.$.updateOfferAddButton.hidden = false;
				this.$.updateOfferPriceButton.hidden = true;
			}
			if (indexUpdatePrice == -1) {
				this.priceUpdateDesc = null;
				this.updateOfferStartDatePrice = null;
				this.updateOfferEndDatePrice = null;
				this.priceUpdateType = null;
				this.priceUpdateSize = null;
				this.priceUpdateUnits = null;
				this.$.updatePriceAmount.value = null;
				this.priceUpdateCurrency = null;
				this.priceUpdatePeriod = null;
				this.$.updateAddPriceCharReserveTime.value = null;
				this.$.updateAddPriceCharReserveBytes.value = null;
				this.$.updateReserveSession.value = null;
				this.updateRedirect = null;
				this.priceAddRoaming = null;
				this.chargingKey = null;
				this.priceUpdateTariff = null;
				this.startTimeUpdate = null;
				this.endTimeUpdate = null;
				this.$.updateCheckIn.checked = false;
				this.$.updateCheckOut.checked = false;
				this.priceUpdateAlt = null;
			} else {
				this.priceUpdateDesc = this.prices[indexUpdatePrice].description;
				if(this.prices[indexUpdatePrice].start || this.prices[indexUpdatePrice].end) {
					this.updateOfferStartDatePrice = this.prices[indexUpdatePrice].start;
					this.updateOfferEndDatePrice = this.prices[indexUpdatePrice].end;
				}
				this.priceUpdateType = this.prices[indexUpdatePrice].priceType;
				this.priceUpdateSize = this.prices[indexUpdatePrice].size;
				switch(this.prices[indexUpdatePrice].unit) {
					case "b":
						this.priceUpdateUnits = "Bytes";
						break;
					case "c":
						this.priceUpdateUnits = "Cents";
						break;
					case "s":
						this.priceUpdateUnits = "Seconds";
						break;
				}
				if(this.prices[indexUpdatePrice].currency) {
					this.priceUpdateCurrency = this.prices[indexUpdatePrice].currency;
				}
				if(this.prices[indexUpdatePrice].amount) {
					this.priceUpdateAmount = this.prices[indexUpdatePrice].amount;
				}
				this.priceUpdatePeriod = this.prices[indexUpdatePrice].period;
				this.priceUpdateAlt = this.prices[indexUpdatePrice].alteration;
				var prodPriceUpdate = this.prices[indexUpdatePrice];
				if(prodPriceUpdate.prodSpecCharValueUse) {
					for (var indexCharVal in prodPriceUpdate.prodSpecCharValueUse) {
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "destPrefixTariffTable") {
							this.priceUpdateTariff = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "roamingTable") {
							this.priceAddRoaming = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "chargingKey") {
							this.chargingKey = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "radiusReserveTime") {
							this.$.updateAddPriceCharReserveTime.value = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "radiusReserveOctets") {
							this.$.updateAddPriceCharReserveBytes.value = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "timeOfDayRange") {
							this.startTimeUpdate = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value.lowerValue.amount;
							this.endTimeUpdate = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value.upperValue.amount;
						}
						if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].name == "callDirection") {
							if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value == "originate") {
								this.$.updateCheckOut.checked = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
							} else if(prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value == "answer") {
								this.$.updateCheckIn.checked = prodPriceUpdate.prodSpecCharValueUse[indexCharVal].value;
							}
						}
					}
				}
			}
		}
	}

	updateAddAltsDialog() {
		if(this.alterations != undefined) {
			function checkUpdateAltName(updateAlts) {
				return updateAlts.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updateAltName').value;
			}
			var indexAlt = this.alterations.findIndex(checkUpdateAltName);
			if(indexAlt != -1) {
				this.$.updateAddAlterationButton.hidden = true;
				this.$.updateOfferAlterationButton.hidden = false;
			} else {
				this.$.updateAddAlterationButton.hidden = false;
				this.$.updateOfferAlterationButton.hidden = true;
			}
			if (indexAlt == -1) {
				this.AltUpdateDesc = null;
				this.updateOfferStartDateAlt = null;
				this.updateOfferEndDateAlt = null;
				this.altUpdateType = null;
				this.$.updateAltSize.value = null;
				this.altUpdateUnits = null;
				this.$.updateAltAmount.value = null;
				this.altUpdateCurr = null;
				this.AltUpdatePer = null;
			} else {
				this.AltUpdateDesc = this.alterations[indexAlt].description;
				if(this.alterations[indexAlt].start || this.alterations[indexAlt].end) {
					this.updateOfferStartDateAlt = this.alterations[indexAlt].start;
					this.updateOfferEndDateAlt = this.alterations[indexAlt].end;
				}
				switch(this.alterations[indexAlt].priceType) {
					case "recurring":
						this.$.updateAltType.selected = 0;
						break;
					case "one_time":
						this.$.updateAltType.selected = 1;
						break;
					case "usage":
						this.$.updateAltType.selected = 2;
						break;
				}
				this.$.updateAltSize.value = this.alterations[indexAlt].size;
				switch(this.alterations[indexAlt].unit) {
					case "b":
						this.$.updateUnitDrop.selected = 0;
						break;
					case "c":
						this.$.updateUnitDrop.selected = 1;
						break;
					case "s":
						this.$.updateUnitDrop.selected = 2;
						break;
				}
				if(this.alterations[indexAlt].currency || this.alterations[indexAlt].amount) {
					this.altUpdateCurr = this.alterations[indexAlt].currency;
					this.$.updateAltAmount.value = this.alterations[indexAlt].amount;
				}
				switch(this.alterations[indexAlt].period) {
					case "hourly":
						this.$.updateAltPeriod.selected = 0;
						break;
					case "daily":
						this.$.updateAltPeriod.selected = 1;
						break;
					case "weekly":
						this.$.updateAltPeriod.selected = 2;
						break;
					case "monthly":
						this.$.updateAltPeriod.selected = 3;
						break;
					case "yearly":
						this.$.updateAltPeriod.selected = 4;
						break;
				}
			}
		}
	}

	_updateProductOffer(event) {
		var ajax =  this.$.updateProductOfferAjax;
		ajax.url = "/catalogManagement/v2/productOffering/" + this.updateOfferName; 
		var offerNew = new Array();
		if(this.updateOfferDesc) {
			var offerDesc = new Object();
			offerDesc.op = "add";
			offerDesc.path = "/description";
			offerDesc.value = this.updateOfferDesc;
			offerNew.push(offerDesc);
		}
		if(this.updateOfferStartDate) {
			var startDateTimeObject = new Object();
			startDateTimeObject.op = "add";
			startDateTimeObject.path = "/validFor/startDateTime";
			startDateTimeObject.value = this.updateOfferStartDate;
			offerNew.push(startDateTimeObject);
		}
		if(this.updateOfferEndDate) {
			var endDateTimeObject = new Object();
			endDateTimeObject.op = "add";
			endDateTimeObject.path = "/validFor/endDateTime";
			endDateTimeObject.value = this.updateOfferEndDate;
			offerNew.push(endDateTimeObject);
		}
		if(this.$.updateReserveSession.value) {
			function checkName(char) {
				return char.name == "radiusReserveSessionTime";
			}
			var res = this.characteristics.findIndex(checkName);
			if(res == -1) {
				var indexChar = "-";
				var reserveSession = new Object();
				reserveSession.op = "add";
				reserveSession.path = "/prodSpecCharValueUse/" + indexChar; 
				var session2Arr = new Array();
				var session2 = new Object();
				session2.default = true;
				session2.value = parseInt(this.$.updateReserveSession.value);
				session2Arr.push(session2);
				var session1 = new Object();
				session1.name = "radiusReserveSessionTime";
				session1.minCardinality = 0;
				session1.maxCardinality = 1;
				session1.productSpecCharacteristicValue = session2Arr;
				var session2 = new Object();
				session2.id = "1";
				session2.href = "/catalogManagement/v2/productSpecification/1";
				session1.productSpecification = session2;
				reserveSession.value = session1;
				offerNew.push(reserveSession);
			} else {
				var indexChar = res.toString();
				var reserveSession = new Object();
				reserveSession.op = "add";
				reserveSession.path = "/prodSpecCharValueUse/" + indexChar + "/productSpecCharacteristicValue/0/value";
				reserveSession.value = parseInt(this.$.updateReserveSession.value);
				offerNew.push(reserveSession);
			}
		}
		if(this.updateRedirect) {
			function checkNameRe(redir) {
				return redir.name == "redirectServer";
			}
			var res = this.characteristics.findIndex(checkNameRe);
			if(res == -1) {
				var indexChar = "-";
				var redirectSer = new Object();
				redirectSer.op = "add";
				redirectSer.path = "/prodSpecCharValueUse/" + indexChar; 
				var redirectSerArr = new Array();
				var redirectSer1 = new Object();
				redirectSer1.value = this.updateRedirect;
				redirectSerArr.push(redirectSer1);
				var redirectSer2 = new Object();
				redirectSer2.name = "redirectServer";
				redirectSer2.minCardinality = 0;
				redirectSer2.maxCardinality = 1;
				redirectSer2.productSpecCharacteristicValue = redirectSerArr;
				var redirectSer1 = new Object();
				redirectSer1.id = "8";
				redirectSer1.href = "/catalogManagement/v2/productSpecification/8";
				redirectSer2.productSpecification = redirectSer1;
				redirectSer.value = redirectSer2;
				offerNew.push(redirectSer);
			} else {
				var indexChar = res.toString();
				var redirectSer = new Object();
				redirectSer.op = "add";
				redirectSer.path = "/prodSpecCharValueUse/" + indexChar + "/productSpecCharacteristicValue/0/value";
				redirectSer.value = this.updateRedirect;
				offerNew.push(redirectSer);
			}
		}
		ajax.body = JSON.stringify(offerNew);
		ajax.generateRequest();
	}

	_updateProductOfferResponse(event) {
		this.$.updateOfferModal.close();
		var sigApp = document.body.querySelector('sig-app');
		var offerList = sigApp.shadowRoot.getElementById('offerList');
		offerList.splice('offers', 0, offerList.offers.length);
		offerList.shadowRoot.getElementById('offerGrid').clearCache();
		var listOffer = document.getElementsByClassName("bundleCheck");
		Array.prototype.forEach.call(listOffer, function(ell) {
			if(ell.checked == true) {
				ell.checked = false;
			}
		});
		var toast = sigApp.shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
	}

	_updateProductOfferError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_updateOfferPrice(event) {
		var ajax =  this.$.updateOfferPriceAjax;
		ajax.method = "PATCH";
		ajax.contentType = "application/json-patch+json";
		ajax.url = "/catalogManagement/v2/productOffering/" + this.updateOfferName; 
		var updatePriceNew = new Array();
		function checkName(price) {
			return price.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updatePriceName').value;
		}
		var indexPrices = this.prices.findIndex(checkName);
		if(this.priceUpdateDesc != this.prices[indexPrices].description) {
			var priceDesc = new Object();
			priceDesc.op = "add";
			priceDesc.path = "/productOfferingPrice/" + indexPrices + "/description";
			priceDesc.value = this.priceUpdateDesc;
			updatePriceNew.push(priceDesc);
		}
		if(this.priceUpdateType != this.prices[indexPrices].priceType) {
			var pricetype = new Object();
			pricetype.op = "add";
			pricetype.path = "/productOfferingPrice/" + indexPrices + "/priceType";
			switch(this.priceUpdateType) {
				case "Recurring":
					pricetype.value = "recurring";
					break;
				case "One Time":
					pricetype.value = "one_time";
					break;
				case "Usage":
					pricetype.value = "usage";
					break;
				case "Tariff":
					pricetype.value = "tariff";
					break;
			}
			updatePriceNew.push(pricetype);
		} 
		if(this.priceUpdateSize != this.prices[indexPrices].size) {
			var priceSize = new Object();
			priceSize.op = "add";
			priceSize.path = "/productOfferingPrice/" + indexPrices + "/unitOfMeasure";
			for(var indexUnit in this.prices) {
				if(this.priceUpdateUnits == "Seconds") {
					this.prices[indexUnit].unit = "s";
				}
				if(this.priceUpdateUnits == "Bytes") {
					this.prices[indexUnit].unit = "b";
				}
				if(this.prices[indexUnit].unit != undefined) {
					var unitDrop = this.prices[indexUnit].unit;
				}
				if(this.priceUpdateSize != undefined) {
					var sizeVal = this.priceUpdateSize;
				}
				if(unitDrop && sizeVal) {
					var len = sizeVal.length;
					var m = sizeVal.charAt(len - 1);
					if(isNaN(parseInt(m))) {
						var s = sizeVal.slice(0, (len - 1));
					} else {
						var s = sizeVal;
					}
					if(unitDrop == "b") {
						if (m == "m") {
							priceSize.value = s + "000000b";
						} else if(m == "g") {
							priceSize.value = s + "000000000b";
						} else if(m == "k") {
							priceSize.value = s + "000b";
						} else {
							priceSize.value = s + "b";
						}
					} else if(unitDrop == "s") {
						var n = Number(s);
						if(m == "m") {
							n = n * 60;
							priceSize.value = n.toString() + "s";
						} else if(m == "h") {
							n = n * 3600;
							priceSize.value = n.toString() + "s";
						} else {
							priceSize.value = n.toString() + "s";
						}
					}
				}
				updatePriceNew.push(priceSize);
			}
		}
		if(this.priceUpdateAmount != this.prices[indexPrices].amount) {
			var priceAmount = new Object();
			priceAmount.op = "add";
			priceAmount.path = "/productOfferingPrice/" + indexPrices + "/price/taxIncludedAmount";
			priceAmount.value = this.priceUpdateAmount;
			updatePriceNew.push(priceAmount);
		}
		if(this.priceUpdatePeriod != this.prices[indexPrices].period) {
			if(!this.$.updatePricePerioddrop.disabled) {
				var priceCharge = new Object();
				priceCharge.op = "add";
				priceCharge.path = "/productOfferingPrice/" + indexPrices + "/recurringChargePeriod";
				switch(this.priceUpdatePeriod) {
					case "Hourly":
						priceCharge.value = "hourly";
						break;
					case "Daily":
						priceCharge.value = "daily";
						break;
					case "Weekly":
						priceCharge.value = "weekly";
						break;
					case "Monthly":
						priceCharge.value = "monthly";
						break;
					case "Yearly":
						priceCharge.value = "yearly";
						break;
				}
				updatePriceNew.push(priceCharge);
			}
		} 
		if(this.prices[indexPrices].prodSpecCharValueUse) {
			if(this.$.updateAddPriceCharReserveTime.value) {
				function checkChar1(charVal) {
					return charVal.name == "radiusReserveTime";
				}
				var res = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkChar1);
				if(res == -1) {
					var indexChar = "-";
					var charReserve = new Object();
					charReserve.op = "add";
					charReserve.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar;
					var resTime1 = new Object();
					resTime1.name = "radiusReserveTime";
					resTime1.valueType = "Number";
					resTime1.minCardinality = 1;
					resTime1.maxCardinality = 1;
					var resTime2Arr = new Array();
					var resTime2 = new Object();
					resTime2.unitOfMeasure = "seconds";
					resTime2.default = true;
					resTime2.value = this.$.updateAddPriceCharReserveTime.value;
					resTime2Arr.push(resTime2);
					resTime1.productSpecCharacteristicValue = resTime2Arr;
					var resTime3 = new Object();
					resTime3.id = "4";
					resTime3.href = "/catalogManagement/v2/productSpecification/4";
					resTime1.productSpecification = resTime3;
					charReserve.value = resTime1;
					updatePriceNew.push(charReserve);
				} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) { 
					if(this.$.updateAddPriceCharReserveTime.value != this.prices[indexPrices].prodSpecCharValueUse[res].value) {
						var indexChar = res.toString();
						var charReserve = new Object();
						charReserve.op = "add";
						charReserve.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar + "/productSpecCharacteristicValue/0/value";
						charReserve.value = this.$.updateAddPriceCharReserveTime.value;
						updatePriceNew.push(charReserve);
					}
				}
			}
			if(this.priceUpdateTariff) {
				function checkName(char) {
					return char.name == "destPrefixTariffTable";
				}
				var resTarriff = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkName);
				if(resTarriff == -1) {
					var indexCharTariff  = "-";
					var destTariff = new Object();
					destTariff.op = "add";
					destTariff.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharTariff;
					var tariff1 = new Object();
					tariff1.name = "destPrefixTariffTable";
					tariff1.minCardinality = 0;
					tariff1.maxCardinality = 1;
					var tariff2Arr = new Array();
					var tariff2 = new Object();
					tariff2.default = true;
					tariff2.value = this.priceUpdateTariff;
					tariff2Arr.push(tariff2);
					tariff1.productSpecCharacteristicValue = tariff2Arr;
					var tariff3 = new Object();
					tariff3.id = "3";
					tariff3.href = "/catalogManagement/v2/productSpecification/3";
					tariff1.productSpecification = tariff3;
					destTariff.value = tariff1;
					updatePriceNew.push(destTariff);
				} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) { 
					if(this.priceUpdateTariff != this.prices[indexPrices].prodSpecCharValueUse[resTarriff].value) {
						var indexCharTariff = resTarriff.toString();
						var destTariff = new Object();
						destTariff.op = "add";
						destTariff.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharTariff  + "/productSpecCharacteristicValue/0/value";
						destTariff.value = this.priceUpdateTariff;
						updatePriceNew.push(destTariff);
					}
				}
			}
			if(this.chargingKey) {
				function checkCharge(charge) {
					return charge.name == "chargingKey";
				}
				var resCharge = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkCharge);
				if(resCharge == -1) {
					var indexCharCharging  = "-";
					var key = new Object();
					key.op = "add";
					key.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharCharging;
					var key1 = new Object();
					key1.name = "chargingKey";
					var key2Arr = new Array();
					var key2 = new Object();
					key2.value = this.chargingKey;
					key2Arr.push(key2);
					key1.productSpecCharacteristicValue = key2Arr;
					var key3 = new Object();
					key3.id = "3";
					key3.href = "/catalogManagement/v2/productSpecification/3";
					key1.productSpecification = key3;
					key.value = key1;
					updatePriceNew.push(key);
				} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) {
					if(this.chargingKey != this.prices[indexPrices].prodSpecCharValueUse[resCharge].value) {
						var indexCharCharging = resCharge;
						var keyE = new Object();
						keyE.op = "add";
						keyE.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharCharging  + "/productSpecCharacteristicValue/0/value";
						keyE.value = this.chargingKey;
						updatePriceNew.push(keyE);
					}
				}
			}
			if(this.priceAddRoaming) {
				function checkName(char) {
					return char.name == "roamingTable";
				}
				var resRoaming = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkName);
				if(resRoaming == -1) {
					var indexCharRoaming = "-";
					var roam = new Object();
					roam.op = "add";
					roam.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharRoaming;
					var roam1 = new Object();
					roam1.name = "roamingTable";
					roam1.maxCardinality = 1;
					var roam2Arr = new Array();
					var roam2 = new Object();
					roam2.default = true;
					roam2.value = this.priceAddRoaming;
					roam2Arr.push(roam2);
					roam1.productSpecCharacteristicValue = roam2Arr;
					var roam3 = new Object();
					roam3.id = "3";
					roam3.href = "/catalogManagement/v2/productSpecification/3";
					roam1.productSpecification = roam3;
					roam.value = roam1;
					updatePriceNew.push(roam);
				} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) {
					if(this.priceAddRoaming != this.prices[indexPrices].prodSpecCharValueUse[resRoaming].value) {
						var indexCharRoaming = resRoaming.toString();
						var roam = new Object();
						roam.op = "add";
						roam.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCharRoaming  + "/productSpecCharacteristicValue/0/value";
						roam.value = this.$.roamingTable.value;
						updatePriceNew.push(roam);
					}
				}
			}
			if(this.$.updateAddPriceCharReserveBytes.value) {
				function checkChar1(charVal) {
					return charVal.name == "radiusReserveOctets";
				}
				var resReserveOctets = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkChar1);
				if(resReserveOctets == -1) {
					var indexChar1 = "-";
					var charResBytes = new Object();
					charResBytes.op = "add";
					charResBytes.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar1;
					var resByte1 = new Object();
					resByte1.name = "radiusReserveOctets";
					resByte1.valueType = "Number";
					resByte1.minCardinality = 1;
					resByte1.maxCardinality = 1;
					var resByte2Arr = new Array();
					var resByte2 = new Object();
					resByte2.unitOfMeasure = "octets";
					resByte2.default = true;
					resByte2.value = this.$.updateAddPriceCharReserveBytes.value;
					resByte1.productSpecCharacteristicValue = resByte2Arr;
					var resByte3 = new Object();
					resByte3.id = "4";
					resByte3.href = "/catalogManagement/v2/productSpecification/4";
					resByte1.productSpecification = resByte3;
					resByte2Arr.push(resByte2);
					charResBytes.value = resByte1; 
					updatePriceNew.push(charResBytes);
				} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) {
					if(this.$.updateAddPriceCharReserveBytes.value != this.prices[indexPrices].prodSpecCharValueUse[resReserveOctets].value) {
						var indexChar1 = resReserveOctets.toString();
						var charResBytes = new Object();
						charResBytes.op = "add";
						charResBytes.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar1 + "/productSpecCharacteristicValue/0/value";
						charResBytes.value = this.$.updateAddPriceCharReserveBytes.value;
						updatePriceNew.push(charResBytes);
					}
				}
			}
			if(this.$.updateCheckIn.checked || this.$.updateCheckOut.checked) {
				function checkCall1(callVal) {
					return callVal.name == "callDirection";
				}
				var resCall = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkCall1);
				if(resCall == -1) {
					var indexCall1 = "-";
					var call = new Object();
					call.op = "add";
					call.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCall1;
					var callDir1 = new Object();
					callDir1.name = "callDirection";
					callDir1.minCardinality = 1;
					callDir1.maxCardinality = 1;
					var callDir2Arr = new Array();
					var callDir2 = new Object();
					callDir2.default = true;
					if(this.$.updateCheckIn.checked) {
						callDir2.value = "answer";
					} else if(this.$.updateCheckOut.checked) {
						callDir2.value = "originate";
					}
					callDir2Arr.push(callDir2);
					callDir1.productSpecCharacteristicValue = callDir2Arr;
					var callDir3 = new Object();
					callDir3.id = "5";
					callDir3.href = "/catalogManagement/v2/productSpecification/5";
					callDir1.productSpecification = callDir3;
					call.value = callDir1;
					updatePriceNew.push(call);
				} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) {
					if(this.$.updateCheckIn.checked != this.prices[indexPrices].prodSpecCharValueUse[resCall].value || this.$.updateCheckOut.checked != this.prices[indexPrices].prodSpecCharValueUse[resCall].value) {
						var indexCall1 = resCall.toString();
						var call = new Object();
						call.op = "add";
						call.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexCall1 + "/productSpecCharacteristicValue/0/value";
						if(this.$.updateCheckIn.checked) {
							call.value = "answer";
						} else if(this.$.updateCheckOut.checked) {
							call.value = "originate";
						}
						updatePriceNew.push(call);
					}
				}
			}
			if(this.startTimeUpdate ||
					this.endTimeUpdate ||
					(this.startTimeUpdate &&
					this.endTimeUpdate)) {
				function checkChar1(charVal) {
					return charVal.name == "timeOfDayRange";
				}
				var resTime = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkChar1);
				if(resTime == -1) {
					var indexChar2 = "-";
					var timeDay = new Object();
					timeDay.op = "add";
					timeDay.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar2;
					var timeRange1 = new Object();
					timeRange1.name = "timeOfDayRange";
					timeRange1.valueType = "Range";
					timeRange1.minCardinality = 0;
					timeRange1.maxCardinality = 1;
					var timeRangeArr = new Array();
					var timeRange2 = new Object();
					var timeRange3 = new Object();
					var timeRangeLower = new Object();
					timeRangeLower.amount = this.startTimeUpdate;
					timeRangeLower.units = "minutes";
					timeRange3.lowerValue = timeRangeLower;
					var timeRangeUpper = new Object();
					timeRangeUpper.amount = this.endTimeUpdate;
					timeRangeUpper.units = "minutes";
					timeRange3.upperValue = timeRangeUpper;
					timeRange2.value = timeRange3;
					timeRange1.productSpecCharacteristicValue = timeRangeArr;
					timeRangeArr.push(timeRange2);
					timeDay.value = timeRange1;
					updatePriceNew.push(timeDay);
				} else if(this.prices[indexPrices].prodSpecCharValueUse.length != 0) {
					if(this.startTimeUpdate != this.prices[indexPrices].prodSpecCharValueUse[resTime].value || this.endTimeUpdate != this.prices[indexPrices].prodSpecCharValueUse[resTime].value) {
						var indexChar2 = resTime.toString();
						var timeDay1 = new Object();
						timeDay1.op = "add";
						timeDay1.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar2 + "/productSpecCharacteristicValue/0/value/lowerValue/amount";
						timeDay1.value = this.startTimeUpdate;
						updatePriceNew.push(timeDay1);
						var timeDayEnd = new Object();
						timeDayEnd.op = "add";
						timeDayEnd.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar2 + "/productSpecCharacteristicValue/0/value/upperValue/amount";
						timeDayEnd.value = this.endTimeUpdate;
						updatePriceNew.push(timeDayEnd);
					}
				}
			}
		}
		ajax.body = JSON.stringify(updatePriceNew);
		ajax.generateRequest();
		this.priceUpdateDesc = null;
		this.priceUpdateSize = null;
		this.priceUpdateType = null;
		this.priceUpdatePeriod = null;
		this.$.updateAddPriceCharReserveTime.value = null;
		this.$.updateDestPrefixTariff.value = null;
		this.priceAddRoaming = null;
		this.chargingKey = null;
		this.$.updateAddPriceCharReserveBytes.value = null;
		this.$.updateCheckIn.checked = false;
		this.$.updateCheckOut.checked = false;
		this.startTimeUpdate = null;
		this.endTimeUpdate = null;
		this.priceUpdateAmount = null;
		this.priceUpdateUnits = null;
	} 

	_updateOfferPriceResponse(event) {
		this.$.updateOfferModal.close();
		var sigApp = document.body.querySelector('sig-app');
		var offerList = sigApp.shadowRoot.getElementById('offerList');
		offerList.splice('offers', 0, offerList.offers.length);
		offerList.shadowRoot.getElementById('offerGrid').clearCache();
		var toast = sigApp.shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
	}

	_updateOfferPriceError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_updateOfferAlteration(event) {
		var ajax =  this.$.updateOfferAlterationAjax;
		ajax.method = "PATCH";
		ajax.contentType = "application/json-patch+json";
		ajax.url = "/catalogManagement/v2/productOffering/" + this.updateOfferName; 
		var updateAlterationNew = new Array();
		function checkAlterationName(alts) {
			return alts.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updateAltName').value;;
		}
		var indexAlt = this.alterations.findIndex(checkAlterationName);
		if(this.$.updateAltName.value != this.alterations[indexAlt].name) {
			var alterationName = new Object();
			alterationName.op = "add";
			alterationName.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/name";
			alterationName.value = this.$.updateAltName.value;
			updateAlterationNew.push(alterationName);
		}
		if(this.AltUpdateDesc != this.alterations[indexAlt].description) {
			var alterationDesc = new Object();
			alterationDesc.op = "add";
			alterationDesc.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/description";
			alterationDesc.value = this.AltUpdateDesc;
			updateAlterationNew.push(alterationDesc);
		}
		if(this.altUpdateType != this.alterations[indexAlt].priceType) {
			var alterationType = new Object();
			alterationType.op = "add";
			alterationType.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/priceType";
			switch(this.$.updateAltType.selected) {
				case 0:
					alterationType.value = "recurring";
					break;
				case 1:
					alterationType.value = "one_time";
					break;
				case 2:
					alterationType.value = "usage";
					break;
			}
			updateAlterationNew.push(alterationType);
		}
		if(this.$.updateAltSize.value) {
			var alterationSize = new Object();
			alterationSize.op = "add";
			alterationSize.path = "/productOfferingPrice/" + indexAlt + "/productOfferPriceAlteration/unitOfMeasure";
			for(var indexUnit1 in this.alterations) {
				if(this.$.updateAltsUnitsdrop.value == "Seconds") {
					this.alterations[indexUnit1].unit = "s";
				}
				if(this.$.updateAltsUnitsdrop.value == "Bytes") {
					this.alterations[indexUnit1].unit = "b";
				}
				var unitDrop = this.alterations[indexUnit1].unit;
				var sizeVal = this.$.updateAltSize.value + unitDrop;
				if(unitDrop && sizeVal) {
					var len = sizeVal.length;
					var m = sizeVal.charAt(len - 1);
					if(isNaN(parseInt(m))) {
						var s = sizeVal.slice(0, (len - 1));
					} else {
						var s = sizeVal.size;
					}
					if(unitDrop == "b") {
						if (m == "m") {
							alterationSize.value = s + "000000b";
						} else if(m == "g") {
							alterationSize.value = s + "000000000b";
						} else if(m == "k") {
							alterationSize.value = s + "000b";
						} else {
							alterationSize.value = s + "b";
						}
					} else if(unitDrop == "s") {
						var n = Number(s);
						if(m == "m") {
							n = n * 60;
							alterationSize.value = n.toString() + "s";
						} else if(m == "h") {
							n = n * 3600;
							alterationSize.value = n.toString() + "s";
						} else {
							alterationSize.value = n.toString() + "s";
						}
					}
				}
				updateAlterationNew.push(alterationSize);
			}
		}
		if(this.$.updateAltAmount.value) {
			var altAmount = new Object();
			altAmount.op = "add";
			altAmount.path = "/productOfferingPrice/" + indexAlt + "/price/taxIncludedAmount";
			altAmount.value = this.$.updateAltAmount.value;
			updateAlterationNew.push(altAmount);
		}
		if(this.$.addalt5drop.value && !this.$.addalt5drop.disabled) {
			var altCharge = new Object();
			altCharge.op = "add";
			altCharge.path = "/productOfferingPrice/" + indexAlt + "/recurringChargePeriod";
			switch(this.$.updateAltPeriod.selected) {
				case 0:
					altCharge.value = "hourly";
					break;
				case 1:
					altCharge.value = "daily";
					break;
				case 2:
					altCharge.value = "weekly";
					break;
				case 3:
					altCharge.value = "monthly";
					break;
				case 4:
					altCharge.value = "yearly";
			}
			updateAlterationNew.push(altCharge);
		} 
		ajax.body = JSON.stringify(updateAlterationNew);
		ajax.generateRequest();
	}

	_updateOfferAlterationResponse(event) {
		this.$.updateOfferModal.close();
		var sigApp = document.body.querySelector('sig-app');
		var offerList = sigApp.shadowRoot.getElementById('offerList');
		offerList.splice('offers', 0, offerList.offers.length);
		offerList.shadowRoot.getElementById('offerGrid').clearCache();
		var toast = sigApp.shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
	}

	_updateOfferAlterationError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	checkPattern() {
		if(this.$.updatePriceUnits.selected == 0) {
			this.$.updatePriceSize.allowedPattern = "[0-9kmg]";
			this.$.updatePriceSize.pattern = "^[0-9]+[kmg]?$";
			this.$.updatePriceSize.disabled = false;
			this.$.updateAddPriceCharReserveBytes.disabled = false;
			this.$.updateAddPriceCharReserveTime.disabled = true;
		} else if(this.$.updatePriceUnits.selected == 1) {
			this.$.updatePriceSize.allowedPattern = "[0-9]";
			this.$.updatePriceSize.pattern = "^[0-9]+$";
			this.$.updatePriceSize.disabled = true;
		} else if(this.$.updatePriceUnits.selected == 2) {
			this.$.updatePriceSize.allowedPattern = "[0-9mh]";
			this.$.updatePriceSize.pattern = "^[0-9]+[mh]?$";
			this.$.updateAddPriceCharReserveTime.disabled = false;
			this.$.updateAddPriceCharReserveBytes.disabled = true;
			this.$.updatePriceSize.disabled = false;
		}
	}

	checkPatternAlt() {
		if(this.$.updateUnitDrop.selected == 0) {
			this.$.updateAltSize.allowedPattern = "[0-9kmg]";
			this.$.updateAltSize.pattern = "^[0-9]+[kmg]?$";
		} else if(this.$.updateUnitDrop.selected == 1) {
			this.$.updateAltSize.allowedPattern = "[0-9]";
			this.$.updateAltSize.pattern = "^[0-9]+$";
		} else if(this.$.updateUnitDrop.selected == 2) {
			this.$.updateAltSize.allowedPattern = "[0-9mh]";
			this.$.updateAltSize.pattern = "^[0-9]+[mh]?$";
		}
	}

	checkRecure() {
		if(this.$.updatePriceType.selected == 0) {
			this.$.updatePricePerioddrop.disabled = false;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = true;
			this.$.priceCents.disabled = false;
			this.$.updateAddPriceCharReserveTime.disabled = true;
			this.$.updateAddPriceCharReserveBytes.disabled = true;
			this.$.updatePriceUnits.selected = 1;
			this.$.updatePriceAmount.disabled = false;
		} else if(this.$.updatePriceType.selected == 1) {
			this.$.updatePricePerioddrop.disabled = true;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = true;
			this.$.priceCents.disabled = false;
			this.$.updateAddPriceCharReserveTime.disabled = true;
			this.$.updateAddPriceCharReserveBytes.disabled = true;
			this.$.updatePriceUnits.selected = 1;
			this.$.updatePriceAmount.disabled = false;
		} else if(this.$.updatePriceType.selected == 2) {
			this.$.updatePricePerioddrop.disabled = true;
			this.$.priceCents.disabled = true;
			this.$.priceBytes.disabled = false;
			this.$.priceSeconds.disabled = false;
			this.$.updatePriceUnits.selected = 0;
			this.$.updatePriceAmount.disabled = false;
		} else if(this.$.updatePriceType.selected == 3) {
			this.$.updatePricePerioddrop.disabled = true;
			this.$.priceCents.disabled = true;
			this.$.priceBytes.disabled = true;
			this.$.priceSeconds.disabled = false;
			this.$.updatePriceUnits.selected = 2;
			this.$.updatePriceAmount.disabled = true;
			this.$.updatePriceAmount.value = null;
		}
	}

	checkRecureAlt() {
		if(this.$.updateAltType.selected == 0) {
			this.$.addalt5drop.disabled = false;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = false;
			this.$.updateUnitDrop.selected = 1;
		} else if(this.$.updateAltType.selected == 1) {
			this.$.addalt5drop.disabled = true;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = false;
			this.$.updateUnitDrop.selected = 1;
		} else if(this.$.updateAltType.selected == 2) {
			this.$.addalt5drop.disabled = true;
			this.$.altBytes.disabled = false;
			this.$.altSeconds.disabled = false;
			this.$.altCents.disabled = true;
			this.$.updateUnitDrop.selected = 0;
		}
	}

	updateAddPrice(event) {
		function updateCheckPriceName(updatePrice) {
			return updatePrice.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updatePriceName').value;
		}
		var updateIndexPrice = this.prices.findIndex(updateCheckPriceName);
		if(updateIndexPrice == -1) {
			var updatePriceNew = new Object();
		} else {
			var updatePriceNew = this.prices[updateIndexPrice];
		}
		updatePriceNew.name = this.$.updatePriceName.value;
		updatePriceNew.description = this.priceUpdateDesc;
		updatePriceNew.start = this.updateOfferStartDatePrice;
		updatePriceNew.end = this.updateOfferEndDatePrice;
		switch(this.$.updatePriceType.selected) {
			case 0:
				updatePriceNew.priceType = "recurring";
				break;
			case 1:
				updatePriceNew.priceType = "one_time";
				break;
			case 2:
				updatePriceNew.priceType = "usage";
				break;
			case 3:
				updatePriceNew.priceType = "tariff";
				break;
		}
		switch(this.$.updatePriceUnits.selected) {
			case 0:
				updatePriceNew.unit = "b";
				break;
			case 1:
				updatePriceNew.unit = "c";
				break;
			case 2:
				updatePriceNew.unit = "s";
				break;
		}
		updatePriceNew.amount = this.priceUpdateAmount;
		updatePriceNew.size = this.priceUpdateSize;
		updatePriceNew.currency = this.priceUpdateCurrency;
		switch(this.$.updatePricePeriod.selected) {
			case 0:
				updatePriceNew.period = "hourly";
				break;
			case 1:
				updatePriceNew.period = "daily";
				break;
			case 2:
				updatePriceNew.period = "weekly";
				break;
			case 3:
				updatePriceNew.period = "monthly";
				break
			case 4:
				updatePriceNew.period = "yearly";
				break;
		}
		var charAddObj = new Object();
		charAddObj.priceCharReserveTime = this.$.updateAddPriceCharReserveTime.value;
		charAddObj.priceCharReserveBytes = this.$.updateAddPriceCharReserveBytes.value;
		charAddObj.timeOfDayStart = this.startTimeUpdate;
		charAddObj.timeOfDayEnd = this.endTimeUpdate;
		updatePriceNew.prodSpecCharValueUse = charAddObj;
		if(this.priceUpdateAlt) {
			function checkAlt(alts) {
				return alts.name == this.priceUpdateAlt;
			}
			updatePriceNew.alteration = this.alterations.findIndex(checkAlt);
		}
		if(updatePriceNew.name
					&& (updatePriceNew.amount || updatePriceNew.updatePriceType == "tariff") 
					&& updatePriceNew.priceType
					&& updatePriceNew.unit) {
			if(updateIndexPrice == -1) {
				this.push('prices', updatePriceNew);
			}
			var ajax =  this.$.updateOfferPriceAjax;
			ajax.method = "PATCH";
			ajax.contentType = "application/json-patch+json";
			ajax.url = "/catalogManagement/v2/productOffering/" + this.updateOfferName; 
			var updatePriceNew1 = new Array();
			function checkName(price) {
				return price.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updatePriceName').value;
			}
			var indexPrices = this.prices.findIndex(checkName);
			if(this.$.updatePriceName.value) {
				var priceNameUp = new Object();
				priceNameUp.op = "add";
				priceNameUp.path =  "/productOfferingPrice/" + "-" + "/name";
				priceNameUp.value = this.$.updatePriceName.value;
				updatePriceNew1.push(priceNameUp);
			}
			if(this.priceUpdateDesc) {
				var priceDesc = new Object();
				priceDesc.op = "add";
				priceDesc.path = "/productOfferingPrice/" + "-" + "/description";
				priceDesc.value = this.priceUpdateDesc;
				updatePriceNew1.push(priceDesc);
			} 
			if(this.priceUpdateType) {
				var pricetype = new Object();
				pricetype.op = "add";
				pricetype.path = "/productOfferingPrice/" + "-" + "/priceType";
				switch(this.$.updatePriceType.selected) {
					case 0:
						pricetype.value = "recurring";
						break;
					case 1:
						pricetype.value = "one_time";
						break;
					case 2:
						pricetype.value = "usage";
						break;
					case 3:
						pricetype.value = "tariff";
						break;
				}
				updatePriceNew1.push(pricetype);
			} 
			if(this.priceUpdateSize) {
				var priceSize = new Object();
				priceSize.op = "add";
				priceSize.path = "/productOfferingPrice/" + "-" + "/unitOfMeasure";
				priceSize.value = this.priceUpdateSize;
				updatePriceNew1.push(priceSize);
			}
			if(this.$.updatePriceAmount.value) {
				var priceAmount = new Object();
				priceAmount.op = "add";
				priceAmount.path = "/productOfferingPrice/" + "-" + "/taxIncludedAmount";
				priceAmount.value = this.$.updatePriceAmount.value;
				updatePriceNew1.push(priceAmount);
			}
			if(this.$.updateAddPriceCharReserveTime.value) {
				var charReserve = new Object();
				charReserve.op = "add";
				charReserve.path = "/productOfferingPrice/" + indexPrices + "/radiusReserveTime";
				charReserve.value = this.$.updateAddPriceCharReserveTime.value;
				updatePriceNew.push(charReserve);
			}
			if(this.$.updateAddPriceCharReserveBytes.value) {
				var charResBytes = new Object();
				charResBytes.op = "add";
				charResBytes.path = "/productOfferingPrice/" + indexPrices + "/radiusReserveOctets";
				charResBytes.value = this.$.updateAddPriceCharReserveBytes.value;
				updatePriceNew.push(charResBytes);
			}
			if(this.startTimeUpdate) {
				function checkChar1(charVal) {
					return charVal.name == "timeOfDayRange";
				}
				var indexChar2 = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkChar1);
				var timeDay = new Object();
				timeDay.op = "add";
				timeDay.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar2 + "/productSpecCharacteristicValue/0/value";
				timeDay.value = this.startTimeUpdate;
				updatePriceNew.push(timeDay);
			}
			if(this.endTimeUpdate) {
				function checkChar1(charVal) {
					return charVal.name == "timeOfDayRange";
				}
				var indexChar2 = this.prices[indexPrices].prodSpecCharValueUse.findIndex(checkChar1);
				var timeDayEnd = new Object();
				timeDayEnd.op = "add";
				timeDayEnd.path = "/productOfferingPrice/" + indexPrices + "/prodSpecCharValueUse/" + indexChar2 + "/productSpecCharacteristicValue/0/value";
				timeDayEnd.value = this.endTimeUpdate;
				updatePriceNew.push(timeDayEnd);
			}
			ajax.body = JSON.stringify(updatePriceNew1);
			ajax.generateRequest();
			this.priceUpdateName = null;
			this.priceUpdateDesc = null;
			this.updateOfferStartDatePrice = null;
			this.priceUpdateSize = null;
			this.$.updatePriceAmount.value = null;
			this.priceUpdateUnits = null;
			this.updateOfferEndDatePrice = null;
			this.priceUpdateType = null;
			this.priceUpdateCurrency = null;
			this.priceUpdatePeriod = null;
			this.$.updateAddPriceCharReserveTime.value = null;
			this.$.updateAddPriceCharReserveBytes.value = null;
			this.priceUpdateTariff = null;
			this.$.updateReserveSession.value = null;
			this.updateRedirect = null;
			this.startTimeUpdate = null;
			this.endTimeUpdate = null;
			this.$.updateCheckIn.checked = false;
			this.$.updateCheckOut.checked = false;
			this.priceUpdateAlt = null;
		} else {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
		}
	}

	updateAddAlteration() {
		function updateCheckAltName(updateAlts) {
			return updateAlts.name == document.body.querySelector('sig-app').shadowRoot.getElementById('updateOffer').shadowRoot.getElementById('updateAltName').value;
		}
		var updateIndexAlt = this.alterations.findIndex(updateCheckAltName);
		if(updateIndexAlt == -1) {
			var updateAltNew = new Object();
		} else {
			var updateAltNew = this.alterations[updateIndexAlt];
		}
		updateAltNew.name = this.$.updateAltName.value;
		updateAltNew.description = this.AltUpdateDesc;
		updateAltNew.start = this.updateOfferStartDateAlt;
		updateAltNew.end = this.updateOfferEndDateAlt;
		switch(this.$.updateAltType.selected) {
			case 0:
				updateAltNew.priceType = "recurring";
				break;
			case 1:
				updateAltNew.priceType = "one_time";
				break;
			case 2:
				updateAltNew.priceType = "usage";
				break;
		}
		switch(this.$.updateUnitDrop.selected) {
			case 0:
				updateAltNew.unit = "b";
				break;
			case 1:
				updateAltNew.unit = "c";
				break
			case 2:
				updateAltNew.unit = "s";
				break;
		}
		updateAltNew.size = this.$.updateAltSize.value;
		switch(this.$.updateAltPeriod.selected) {
			case 0:
				updateAltNew.period = "hourly";
				break;
			case 1:
				updateAltNew.period = "daily";
				break;
			case 2:
				updateAltNew.period = "weekly";
				break;
			case 3:
				updateAltNew.period = "monthly";
				break
			case 4:
				updateAltNew.period = "yearly";
				break;
		}
		updateAltNew.currency = this.altUpdateCurr;
		updateAltNew.amount= this.$.updateAltAmount.value;
		if(updateAltNew.name
					&& updateAltNew.priceType
					&& updateAltNew.unit
					&& (updateAltNew.amount || updateAltNew.amount == 0)) {
			if(updateIndexAlt == -1) {
				this.push('alterations', updateAltNew);
			}
			this.AltUpdateName = null
			this.AltUpdateDesc = null;
			this.updateOfferStartDateAlt = null;
			this.updateOfferEndDateAlt = null;
			this.altUpdateType = null;
			this.$.updateAltSize.value = null;
			this.altUpdateCurr = null;
			this.$.updateAltAmount.value = null;
		} else {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
		}
	}

	_deleteOffer(event) {
		this.$.deleteOfferAjax.url = "/catalogManagement/v2/productOffering/"
					+ this.updateOfferName;
		this.$.deleteOfferAjax.generateRequest();
	}

	_deleteOfferResponse(event) {
		this.$.updateOfferModal.close();
		var sigApp = document.body.querySelector('sig-app');
		var offerList = sigApp.shadowRoot.getElementById('offerList');
		offerList.splice('offers', 0, offerList.offers.length);
		offerList.shadowRoot.getElementById('offerGrid').clearCache();
		var toast = sigApp.shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
	}

	_deleteOfferError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_onLoadingChanged(event) {
		if (this.$.updateProductOfferAjax.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}

	cancelDialog() {
		this.set('prices', []);
		this.set('alterations', []);
		var list = document.getElementsByClassName("bundleCheck");
		Array.prototype.forEach.call(list, function(el) {
			if(el.checked == true) {
				el.checked = false;
			}
		});
		this.updateOfferName = null;
		this.updateOfferDesc = null;
		this.updateOfferSpec = null;
		this.updateOfferStartDate = null;
		this.updateOfferEndDate = null;
		this.$.updateAddPriceChars.hide();
		this.$.addBundleUpdate.hide();
		this.priceUpdateName = null;
		this.priceUpdateDesc = null;
		this.updateOfferStartDatePrice = null;
		this.updateOfferEndDatePrice = null;
		this.priceUpdateType = null;
		this.priceUpdateUnits = null;
		this.priceUpdateAmount = null;
		this.priceUpdateSize = null;
		this.priceUpdateCurrency = null;
		this.priceUpdatePeriod = null;
		this.priceUpdateAlt = null;
		this.AltUpdateName = null;
		this.AltUpdateDesc = null;
		this.updateOfferStartDateAlt = null;
		this.updateOfferEndDateAlt = null;
		this.altUpdateType = null;
		this.$.updateAltSize.value = null;
		this.altUpdateCurr = null;
		this.$.updateAltAmount.value = null;
		this.AltUpdatePer = null;
		this.altUpdateUnits = null;
		this.$.updateAddPriceCharReserveTime.value = null;
		this.$.updateAddPriceCharReserveBytes.value = null;
		this.priceUpdateTariff = null;
		this.priceAddRoaming = null;
		this.chargingKey = null;
		this.$.updateReserveSession.value = null;
		this.updateRedirect = null;
		this.startTimeUpdate = null;
		this.endTimeUpdate = null;
		this.$.updateCheckIn.checked = null;
		this.$.updateCheckOut.checked = null;
		this.$.updateOfferModal.close();
	}
}

window.customElements.define('sig-offer-update', offerUpdate);
