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
import './style-element.js';

class offerAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog
					class="dialog"
					id="addBucketModal"
					modal>
				<app-toolbar>
					<h3>Add Bucket</h3>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						id="addProduct"
						name="product"
						value="{{proProduct}}"
						label="Product">
				</paper-input>
				<div>
					<paper-dropdown-menu
							value="{{proUnit}}"
							label="Units">
						<paper-listbox
								id="addUnitsBucket"
								slot="dropdown-content">
							<paper-item>
									Bytes
							</paper-item>
							<paper-item>
									Cents
							</paper-item>
							<paper-item>
									Seconds
							</paper-item>
						</paper-listbox>
					</paper-dropdown-menu>
				</div>
				<paper-input
						id="amount"
						name="amount"
						type="text"
						allowed-pattern="[0-9kmg]"
						pattern="^[0-9]+[kmg]?$"
						label="Amount"
						value="{{proAmount}}"
						auto-validate>
				</paper-input>
				<div class="buttons">
					<paper-button
							raised
							class="submit-button"
							on-tap="_bucketAddSubmit">
						Add
					</paper-button>
					<paper-button dialog-dismiss
							class="cancel-button"
							on-tap="canBucket">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<paper-dialog
					class="dialog"
					id="deleteBucketModal" modal>
				<app-toolbar>
					<h2>Delete Bucket</h2>
				</app-toolbar>
				<paper-input
						id="deleteBucId"
						value="{{delBuc}}"
						name="BucketId"
						label="Bucket Id"
						required
						disabled>
				</paper-input>
				<div class="buttons">
					<paper-button raised
							id="bucDelButton"
							on-tap="bucketDelete"
							class="delete-buttons">
						Delete
					</paper-button>
					<paper-button dialog-dismiss
							class="cancel-button"
							onclick="deleteProductModal.close()">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="deleteBucketAjax"
					url="/balanceManagement/v1/bucket/"
					method="delete"
					on-response="_deleteBucketResponse"
					on-error="_deleteBucketError">
			</iron-ajax>
			<iron-ajax
					id="addBucketAjax"
					method = "post"
					content-type="application/json"
					on-loading-changed="_onLoadingChanged"
					on-response="_addBucketResponse"
					on-error="_addBucketError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
				loading: {
				type: Boolean,
				value: false
			},
			proProduct: {
				type: String
			},
			proAmount: {
				type: String
			},
			delBuc: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	bucketDelete(event) {
		var delBucket = this.delBuc;
		var deleteAjax = this.$.deleteBucketAjax;
		deleteAjax.url = "/balanceManagement/v1/bucket/" + delBucket;
		deleteAjax.generateRequest();
	}

	_deleteBucketResponse(event) {
		this.$.deleteBucketModal.close();
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-bucket-list').shadowRoot.getElementById('balanceBucketGrid').clearCache();
	}

	_bucketAddSubmit(event) {
		var ajaxBucket = this.$.addBucketAjax;
		var bucketTop = {name: "channel"};
		var bucketUnits;
		var bucketAmount;
		if(this.proAmount) {
			if(this.proUnit == "Bytes") {
				bucketUnits = "octets";
			}
			else if(this.proUnit == "Cents") {
				bucketUnits = "cents";
			}
			else if(this.proUnit == "Seconds") {
				bucketUnits = "seconds";
			} else {
				bucketUnits = "cents";
			}
			if(bucketUnits && this.proAmount) {
				var size = this.proAmount;
				var len = size.length;
				var m = size.charAt(len - 1);
				if(isNaN(parseInt(m))) {
					var s = size.slice(0, (len - 1));
				} else {
						var s = size;
				}
				if(bucketUnits == "octets") {
					if (m == "m") {
						bucketAmount = s + "000000b";
					} else if(m == "g") {
						bucketAmount = s + "000000000b";
					} else if(m == "k") {
						bucketAmount = s + "000b";
					} else {
						bucketAmount = s + "b";
					}
				} else if(bucketUnits == "cents") {
					bucketAmount = this.proUnit; 
				} else if(bucketUnits == "seconds") {
					var n = Number(s);
					if(m == "m") {
						n = n * 60;
						bucketAmount = n.toString() + "s";
					} else if(m == "h") {
						n = n * 3600;
						bucketAmount = n.toString() + "s";
					} else {
						bucketAmount = n.toString() + "s";
					}
				}
			bucketTop.amount = {units: bucketUnits, amount: bucketAmount};
			}
		}
		var proId = this.proProduct;
		bucketTop.product = {id: proId,
					href: "/productInventoryManagement/v2/product/" + proId};
		ajaxBucket.headers['Content-type'] = "application/json";
		ajaxBucket.body = bucketTop;
		ajaxBucket.url="/balanceManagement/v1/product/" + proId + "/balanceTopup";
		ajaxBucket.generateRequest();
		this.$.addBucketModal.close();
		this.proAmount = null;
		this.$.addProduct.value = null;
		this.$.addUnitsBucket.value = null;
	}

	canBucket() {
		this.proAmount = null;
		this.$.addProduct.value = null;
		this.$.addUnitsBucket.value = null;
	}

	_addBucketResponse(event) {
		this.$.addBucketModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('bucketList').shadowRoot.getElementById('balanceBucketGrid').clearCache();
	}
}

window.customElements.define('sig-bucket-add', offerAdd);
