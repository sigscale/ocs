<!--  vim: set ts=3:  -->
<link rel="import" href="polymer/polymer.html">
<link rel="import" href="i18n-msg/i18n-msg.html">
<link rel="import" href="i18n-msg/i18n-msg-behavior.html">
<link rel="import" href="paper-dialog/paper-dialog.html">
<link rel="import" href="paper-toolbar/paper-toolbar.html">
<link rel="import" href="paper-tabs/paper-tabs.html">
<link rel="import" href="paper-input/paper-input.html">
<link rel="import" href="paper-item/paper-icon-item.html">
<link rel="import" href="paper-item/paper-item-body.html">
<link rel="import" href="paper-tooltip/paper-tooltip.html">
<link rel="import" href="paper-button/paper-button.html">
<link rel="import" href="paper-toggle-button/paper-toggle-button.html" >
<link rel="import" href="paper-toast/paper-toast.html">
<link rel="import" href="paper-styles/color.html">
<link rel="import" href="iron-ajax/iron-ajax.html">
<link rel="import" href="iron-pages/iron-pages.html">
<link rel="import" href="paper-checkbox/paper-checkbox.html">
<link rel="import" href="iron-icons/iron-icons.html">
<link rel="import" href="iron-icons/communication-icons.html">
<link rel="import" href="paper-tags/paper-tags-input.html">

<dom-module id="sig-product-add">
	<template>
		<style is="custom-style">
			paper-dialog {
				overflow: auto;
			}
			paper-input {
				--paper-input-container-focus-color: var(--paper-yellow-900);
			}
			paper-toolbar {
				margin-top: 0px;
				color: white;
				background-color: #bc5100;
			}
			paper-item-body {
				--paper-item-body-secondary: {
					font-weight: bold;
					font-size: larger;
				}
			}
			paper-toast.error {
				background-color: var(--paper-red-a400);
			}
			paper-toggle-button {
				--paper-toggle-button-checked-bar-color: #ffb04c;
				--paper-toggle-button-checked-button-color: var(--paper-yellow-900);
			}
			paper-checkbox {
				--paper-checkbox-checked-color: #ffb04c;
				--paper-checkbox-checkmark-color: var(--paper-yellow-900);
			}
			.toggle {
				display:inline-block;
			}
			.generate {
				display: inline-block;
				width: 8em;
			}
			.identity {
				display: inline-block;
			}
			.password {
				display: inline-block;
			}
			.add-button {
				background-color: var(--paper-lime-a700);
				color: black;
				float: right;
				width: 8em;
			}
			.delete-buttons {
				background: #EF5350;
				color: black;
			}
			.cancel-button {
				color: black;
			}
			.generated {
				padding: 10px;
				overflow: auto;
			}
			.close {
				background-color: var(--paper-lime-a700);
				color: black;
				float: right;
				width: 5em;
			}
		</style>
		<paper-dialog id="addProductInvenModal" modal>
			<paper-toolbar>
				<h2>Add Product</h2>
			</paper-toolbar>
			<div>
				<paper-dropdown-menu
						id="addProDrop"
						label="[[i18n.proOffer]]">
					<paper-listbox
							id="addProDropList"
							slot="dropdown-content"
							class="dropdown-content">
						<template is="dom-repeat" items="[[offers]]">
							<paper-item>
								{{item}}
							</paper-item>
						</template>
					</paper-listbox>
				</paper-dropdown-menu>
				<paper-tags-input
					id="addService1"
					label="[[i18n.servId]]"
					items="[]">
				</paper-tags-input>
			</div>
			<div class="buttons">
				<paper-button dialog-confirm
					raised
					class="add-button"
					on-tap="_productinvenAddSubmit">
					<i18n-msg msgid="submit">
						Submit
					</i18n-msg>
				</paper-button>
				<paper-button dialog-dismiss
					class="cancel-button"
					onclick="addProductInvenModal.close()">
					<i18n-msg msgid="cancel">
						Cancel
					</i18n-msg>
				</paper-button>
			</div>
		</paper-dialog>
		<paper-dialog
				id="deleteProductModal" modal>
			<paper-toolbar>
				<h2>Delete Product</h2>
			</paper-toolbar>
			<div>
				<paper-input
						id="deleteProId"
						name="ProductId"
						label="Product Id"
						required
						disabled>
				</paper-input>
			</div>
			<div class="buttons">
				<paper-button raised
						id="proDelButton"
						on-tap="productDelete"
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
				id="deleteProductAjax"
				url="/productInventoryManagement/v2/product/"
				method="delete"
				on-response="_deleteProductResponse"
				on-error="_deleteProductrror">
		</iron-ajax>
		<iron-ajax
				id="addProductAjax"
				url="/productInventoryManagement/v2/product"
				method = "post"
				content-type="application/json"
				on-loading-changed="_onLoadingChanged"
				on-response="_addProductResponse"
				on-error="_addProductrror">
		</iron-ajax>
	</template>
	<script>
		Polymer ({
			is: 'sig-product-add',
			behaviors: [i18nMsgBehavior],
			properties: {
				selected: {
					type: Number,
					value: 0
				},
				offers: {
					type: Array,
					value: function() {
						return []
					}
				},
				product: {
					type: String,
				}
			},
			productDelete: function(event) {
				var delProduct = this.$.deleteProId.value;
				var deleteAjax = this.$.deleteProductAjax;
				deleteAjax.url = "/productInventoryManagement/v2/product/" + delProduct;
				deleteAjax.generateRequest();
			},
			_deleteProductResponse: function(event) {
				this.$.deleteProductModal.close();
				document.getElementById('getProductInventory').generateRequest();
			},
			_productinvenAddSubmit: function(event) {
				var ajaxPro = this.$.addProductAjax;
				var productRes = new Object();
				var productAdd = new Object();
				productAdd.id = this.$.addProDrop.value;
				productAdd.name = this.$.addProDrop.value;
				productAdd.href = "/catalogManagement/v2/productOffering/" + this.$.addProDrop.value; 
				productRes.productOffering = productAdd;
				var productServi = new Array();
				var services = this.$.addService1.items
				for(var index in services) {
					bla = new Object();
					bla.id = services[index];
					bla.href = "/serviceInventoryManagement/v2/service/" + services[index];
					productServi.push(bla);
				}
				productRes.realizingService= productServi;
				ajaxPro.body = productRes;
				ajaxPro.generateRequest();
				this.$.addProDrop.value = null;
				this.$.addService1.value = null;
				this.$.addProductInvenModal.close();
			},
			_addProductResponse: function(event) {
				this.$.addProductInvenModal.close();
				document.getElementById("productInventoryGrid").clearCache();
				document.getElementById("addSubscriberToastSuccess").open();
			},
			_addProductrror: function(event) {
				document.getElementById("addSubscriberToastError").text = event.detail.request.xhr.statusText;
				document.getElementById("addSubscriberToastError").open();
			},
			_onLoadingChanged: function(event) {
				if (this.$.addProductAjax.loading) {
					document.getElementById("progress").disabled = false;
				} else {
					document.getElementById("progress").disabled = true;
				}
			},
			subcheckboxchanged: function(event) {
				if (event.target.checked) {
					this.$.addSubscriberPassword.disabled = true;
				} else {
					this.$.addSubscriberPassword.disabled = false;
				}
			},
			subcheckchanged: function(event) {
				if (event.target.checked) {
					this.$.addSubscriberId.disabled = true;
				} else {
					this.$.addSubscriberId.disabled = false;
				}
			}
		});
	</script>
</dom-module>
