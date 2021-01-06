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
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/iron-icons/iron-icons.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/iron-collapse/iron-collapse.js';
import '@polymer/paper-icon-button/paper-icon-button.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import './style-element.js';

class ocsHelp extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="helpdialog" id="helpDrop">
				<paper-icon-item class="menu-trigger menuitem"
					on-click="_collapseLogsContact">
					<iron-icon icon="my-icons:contactUs" item-icon></iron-icon>
						Contact Us
				</paper-icon-item>
				<iron-collapse id="contact">
					<div>
						<paper-icon-button
							icon="my-icons:call">
						</paper-icon-button>
						<a href="tel:+639208370397">+639208370397</a>
					</div>
					<div>
						<paper-icon-button
							icon="my-icons:email">
						</paper-icon-button>
						<a target="_blank" href="mailto:info@sigscale.org">info@sigscale.org</a>
					</div>
				</iron-collapse>
				<paper-icon-item class="menu-trigger menuitem">
					<iron-icon icon="my-icons:feedback" item-icon></iron-icon>
					<a target="_blank" href="https://sigscale.atlassian.net/secure/CreateIssue!default.jspa?pid=10100&issuetype=10000">
						Send Feedback
					</a>
				</paper-icon-item>
				<paper-icon-item class="menu-trigger menuitem">
					<iron-icon icon="my-icons:api" item-icon></iron-icon>
					<a target="_blank" href="/doc/index.html">
						API Docs
					</a>
				</paper-icon-item>
				<paper-icon-item
					   class="menu-trigger menuitem"
						on-click="helpId">
					<iron-icon icon="my-icons:help" item-icon></iron-icon>
						Help
				</paper-icon-item>
				<paper-icon-item class="menu-trigger menuitem" on-click="about">
					<iron-icon icon="my-icons:about"></iron-icon>
						About
				</paper-icon-item>
			</paper-dialog>
			<paper-dialog class="helpdialog" id="helpMenu">
				<app-toolbar>
						Help
				</app-toolbar>
				<paper-icon-item
						class="icon-style"
						on-click="product">
					<iron-icon icon="my-icons:assignment" item-icon></iron-icon>
					Product
				</paper-icon-item>
				<paper-icon-item
						class="icon-style"
						on-click="addSubQue">
					<iron-icon icon="my-icons:assignment" item-icon></iron-icon>
					Subscribers
				</paper-icon-item>
				<paper-icon-item
						class="icon-style"
						on-click="addClientQue">
					<iron-icon icon="my-icons:assignment" item-icon></iron-icon>
					Clients
				</paper-icon-item>
				<paper-icon-item
						class="icon-style"
						on-click="addUserQue">
					<iron-icon icon="my-icons:assignment" item-icon></iron-icon>
					Users
				</paper-icon-item>
				<paper-icon-item
						class="icon-style"
						on-click="toolQue">
					<iron-icon icon="my-icons:assignment" item-icon></iron-icon>
					Toolbar
				</paper-icon-item>
				<paper-icon-item
						class="icon-style"
						on-click="filter">
					<iron-icon icon="icons:assignment" item-icon></iron-icon>
					Filtering
				</paper-icon-item>
			</paper-dialog>
			<paper-dialog class="helpdialog" id="helpToolbar">
				<paper-dialog-scrollable>
					<app-toolbar>
						<paper-icon-button
								icon="my-icons:arrow-back"
								on-click="helpBack">
						</paper-icon-button>
						<h2>Toolbar</h2>
					</app-toolbar>
					<paper-icon-item>
						<iron-icon icon="icons:assignment" slot="item-icon"></iron-icon>
						<h2>Toolbar</h2>
					</paper-icon-item>
					<p>The <b>Toolbar</b> appears across the top of the screen in
					all views. In the center of the toolbar is the title of the
					application:</p>
					<img alt="screenshot" src="../images/toolbar.png" height="36" width="320">
					<p>To the left of the title is the <b>Navigation Menu</b> icon:
					<iron-icon class="iconHelp" icon="menu"></iron-icon>. Selecting
					this opens the <b>Navigation Drawer</b> which contains the main
					menu for navigating between views within the application.</p>
					<p>To the right of the title is the <b>Refresh</b> icon:
					<iron-icon class="iconHelp" icon="refresh"></iron-icon>. Selecting
					this will get new data from OCS for the current view.</p>
					<p>On the far right of the toolbar is the <b>Overflow Menu</b> icon:
					<iron-icon class="iconHelp" icon="icons:more-vert"></iron-icon>.
					Selecting this will open a menu providing access to <b>Help</b>
					and <b>Support</b></p>.
					<h3>Progress Bar</h3>
					<p>When loading data from OCS takes a few seconds a progress
					bar appears underneath the toolbar to indicate that it is
					working on loading the page requested:</p>
					<img alt="screenshot" src="../images/progress.png" height="96" width="298">
				</paper-dialog-scrollable>
			</paper-dialog>
			<paper-dialog class="helpdialog" id="helpSubscribers">
				<app-toolbar>
					<paper-icon-button
							icon="my-icons:arrow-back"
							on-click="helpBack">
					</paper-icon-button>
					<h2>Subscribers</h2>
				</app-toolbar>
				<paper-icon-item>
					<iron-icon icon="icons:assignment" slot="item-icon"></iron-icon>
		 			<h2>Subscribers</h2>
				</paper-icon-item>
				<paper-dialog-scrollable>
					<p>To access the <b>Subscribers</b> view first open the navigation
					drawer by selecting it's icon
					<iron-icon class="iconHelp" icon="menu"></iron-icon>
					on the left of the toolbar at top. Select the
					<iron-icon class="iconHelp" icon="device:devices"></iron-icon>
					<b>Subscribers</b> view from the navigation menu.</p>
					<p><b>Subscribers</b> are the entities which subscribe to
					communications services. A subscriber is commonly a person using a
					smartphone, tablet or laptop to access a Wi-Fi network.</p>
					<h4>Authentication</h4>
					<p>Each subscibers must have a unique <b>Identifier</b>. A
					<b>Password</b> is used by OCS for <b>authentication</b> of
					access requests. OCS supports WPA2 EAP with PWD or TTLS methods.</p>
					<h4>Authorization</h4>
					<p>Authenticated subscribers must also be <b>authorized</b>
					for received services. The <b>Enabled</b> flag may be used to
					temporarily suspend an account without changing other settings.</p>
					<p>An account <b>Balance</b> is decremented as services are
					consumed by the subscriber. Authorization will fail when the
					balance is not positive.</p>
					<p> Optional service <b>attributes</b> may be defined for each
					subscriber:</p>
					<dl>
						<dt><b>Receive Data Rate</b></dt>
							<dd>Place a maximum data rate (bps) limit on subscriber sessions.</dd>
						<dt><b>Transmit Data Rate</b></dt>
							<dd>May also be defined when an asymetrical rate limit is required.</dd>
						<dt><b>Session Timeout</b></dt>
							<dd>Time (secs) after which sessions will be reauthenticated.</dd>
						<dt><b>Update Interval</b></dt>
							<dd>Enables interim accounting requests (secs).</dd>
						<dt><b>Class</b></dt>
							<dd>A label which may be used in rating.</dd>
					</dl>
					<p>The two timers may be used to configure the granularity of
					authorization where more frequent updates result in lower negative
					balances at the cost of increased processing and request
					transmissions.</p>
					<h3>Add a Subscriber</h3>
					<p>Select the floating action button
					<iron-icon class="iconHelp" icon="add"></iron-icon> at the bottom right
					of the view. A <b>Product List</b> dropdown menu will appear. When selects
					the product from the list it will redirect you to <b>Add Subscriber</b> dialog.
					Enter the details for a new subscriber and select the <b>SUBMIT</b>
					button. If the <b>Generate</b> check-box is selected for <b>Identity</b>
					or <b>Password</b> OCS will create unique values which will be provided
					in a dialog after the request is successful.</p> <h3>Update a Subscriber</h3>
					<p>Selecting a row in the Subscriber view will open a dialog allowing
					updating any fields. There are two tabs in the dialog toolbar used to
					select <b>Authentication</b> or <b>Authorization</b> fields. After
					providing new values in either or both tabs select the <b>UPDATE</b>
					button.</p>
					<h3>Delete a Subscriber</h3>
					<p>Selecting a row in the Subscriber view will open a dialog which
					includes a <b>DELETE</b> button.</p>
				</paper-dialog-scrollable>
			</paper-dialog>
			<paper-dialog class="helpdialog" id="helpClients">
				<app-toolbar>
					<paper-icon-button
						icon="my-icons:arrow-back"
						on-click="helpBack">
					</paper-icon-button>
					<h2>Clients</h2> 
				</app-toolbar>
				<paper-icon-item>
					<iron-icon icon="icons:assignment" slot="item-icon"></iron-icon>
					<h2>Clients</h2> 
				</paper-icon-item>
				<paper-dialog-scrollable>
					<p>To access the <b>Clients</b> view first open the navigation drawer
					by selecting it's icon <iron-icon class="iconHelp" icon="menu"></iron-icon>
					on the left of the toolbar at top. Select the
					<iron-icon class="iconHelp" icon="hardware:router"></iron-icon>
					<b>Clients</b> view from the navigation menu.</p>
					<p><b>Clients</b> are Network Access Servers (<b>NAS</b>) which
					delegate Authentication, Authorization and Accounting (<b>AAA</b>)
					to OCS.  A typical NAS is a wireless access point (i.e. Wi-Fi
					router). Clients must be trusted by the OCS so each one must be
					defined here.</p>
					<dl>
						<dt><b>Address</b></dt>
							<dd>Must match the origin of access requests from the client.</dd>
						<dt><b>Identifier</b></dt>
							<dd>The NAS-Identifier received from the client (read only).</dd>
						<dt><b>Secret</b></dt>
							<dd>A shared secret configured in the client to authenticate
							requests and responses.</dd>
						<dt><b>Protocol</b></dt>
							<dd>The AAA protocol used with the client (RADIUS/DIAMETER).</dd>
						<dt><b>Port</b></dt>
							<dd>The UDP port which the client listens on for incoming requests
							from OCS to initiate disconnection of ongoing sessions
							(optional).</dd>
					</dl>
					<h3>Add a Client</h3>
					<p>Select the floating action button
					<iron-icon class="iconHelp" icon="add"></iron-icon> at the bottom right
					of the view. An <b>Add Client</b> dialog will appear. Enter details
					for a new client and select the <b>SUBMIT</b> button. If the
					<b>Generate</b> check-box is selected for <b>Secret</b> OCS will
					create a unique value which will be provided in a dialog after the
					request is successful.</p>
					<h3>Update a Client</h3>
					<p>Selecting a row in the Clients view will open a dialog allowing
					updating any fields. There are two tabs in the dialog toolbar used
					to select <b>Authentication</b> or <b>Properties</b> fields. After
					providing new values in either or both tabs select the <b>UPDATE</b>
					button.</p>
					<h3>Delete a Client</h3>
					<p>Selecting a row in the Clients view will open a dialog which
					includes a <b>DELETE</b> button.</p>
				</paper-dialog-scrollable>
			</paper-dialog>
			<paper-dialog class="helpdialog" id="helpLogs">
				<app-toolbar>
					<paper-icon-button
						icon="my-icons:arrow-back"
						on-click="helpBack">
					</paper-icon-button>
					<h2>Logs</h2>
				</app-toolbar>
				<paper-icon-item>
					<iron-icon icon="icons:assignment" slot="item-icon"></iron-icon>
					<h2>Logs</h2>
				</paper-icon-item>
				<paper-dialog-scrollable>
					<p>To access log views first open the navigation drawer
					by selecting it's icon <iron-icon class="iconHelp" icon="menu"></iron-icon>
					on the left of the toolbar at top. Select the
					<iron-icon class="iconHelp" icon="icons:history"></iron-icon>
					<b>Logs</b> submenu from the navigation menu.</p>
					<p>Activity logs provide persistent storage of chronologically
					ordered event details. For AAA requests all attributes of RADIUS
					or DIAMETER requests and responses are logged. Seperate logs
					are used for AAA <b>Access</b> and <b>Accounting</b> requests.
					All HTTP requests on REST API and web user interface are logged
					in <b>Common Log Format</b>.  OCS also generates Internet Protocol
					Detail Record (IPDR) logs which is an industry standard format
					for inter-operator exchange of billing records used with roaming
					agreements.</p>
					<h3>Access</h3>
					<p>Select the <iron-icon class="iconHelp" icon="device:data-usage"></iron-icon>
					<b>Access</b> view from the submenu in the navigation drawer.</p>
					<dl>
						<dt><b>Timestamp</b></dt>
							<dd>The time and date the event was recorded by OCS in ISO8601
							format universal coordinated time (UTC).</dd>
						<dt><b>Client</b></dt>
							<dd>IP address of the NAS which sent the Access-Request.</dd>
						<dt><b>Client Identity</b></dt>
							<dd>Identify of the NAS which sent the Access-Request.</dd>
						<dt><b>Called Station</b></dt>
							<dd>Identifies the subscriber's device (MAC address).</dd>
						<dt><b>Username</b></dt>
							<dd>Identifies the subscriber.</dd>
						<dt><b>Type</b></dt>
							<dd>Indicates the response (accept/reject).</dd>
					</dl>
					<h3>Accounting</h3>
					<p>Select the <iron-icon class="iconHelp" icon="device:data-usage"></iron-icon>
					<b>Accounting</b> view from the submenu in the navigation drawer.</p>
					<dl>
						<dt><b>Timestamp</b></dt>
							<dd>The time and date the event was recorded by OCS in ISO8601
							format universal coordinated time (UTC).</dd>
						<dt><b>Client</b></dt>
							<dd>Identifies the NAS which sent the Accounting-Request.</dd>
						<dt><b>Duration</b></dt>
							<dd>The session duration (secs).</dd>
						<dt><b>In</b></dt>
							<dd>The incoming data traffic (bytes).</dd>
						<dt><b>Out</b></dt>
							<dd>The outgoing data traffic (bytes).</dd>
						<dt><b>Username</b></dt>
							<dd>Identifies the subscriber.</dd>
						<dt><b>Type</b></dt>
							<dd>Indicates the request type (start/stop/interim/on/off).</dd>
					</dl>
					<h3>IPDR</h3>
					<p>Select <iron-icon class="iconHelp" icon="device:data-usage"></iron-icon>
					<b>IPDR</b> from the submenu in the navigation drawer.</p>
					<p>A file selction dialog will appear with a list of available
					IPDR logs.  Filenames are ISO8601 timestamps (UTC) indicating
					time of file creation. Typically IPDR files will contain the
					previous day's events. Select a file to open the <b>IPDR</b>
					view.</p>
					<dl>
						<dt><b>Creation Time</b></dt>
							<dd>The time and date the event was recorded by OCS in ISO8601
							format universal coordinated time (UTC).</dd>
						<dt><b>Seq</b></dt>
							<dd>is a unique sequence number within this IPDR log file.</dd>
						<dt><b>Username</b></dt>
							<dd>Identifies the subscriber.</dd>
						<dt><b>Session</b></dt>
							<dd>Identifies the session.</dd>
						<dt><b>Calling Station</b></dt>
							<dd>Identifies the subscriber's device (MAC address).</dd>
						<dt><b>Called Station ID</b></dt>
							<dd>Identifies the access point (MAC:SSID).</dd>
						<dt><b>NAS Address</b></dt>
							<dd>IP address of the client.</dd>
						<dt><b>NAS ID</b></dt>
							<dd>NAS-Identifier of the client.</dd>
						<dt><b>Duration</b></dt>
							<dd>The duration of the session (secs).</dd>
						<dt><b>Input</b></dt>
							<dd>The incoming data traffic (bytes).</dd>
						<dt><b>Output</b></dt>
							<dd>The outgoing data traffic (bytes).</dd>
						<dt><b>Start Time</b></dt>
							<dd>The time the session started.</dd>
						<dt><b>End Time</b></dt>
							<dd>The time the session ended.</dd>
						<dt><b>Cause</b></dt>
							<dd>Indicates the reason the session ended.</dd>
					</dl>
					<h3>HTTP</h3>
					<p>Select the <iron-icon class="iconHelp" icon="device:data-usage"></iron-icon>
					<b>HTTP</b> view from the submenu in the navigation drawer.</p>
					<dl>
						<dt><b>DateTime</b></dt>
							<dd>A timestamp for the event in Common Log Format.</dd>
						<dt><b>Host</b></dt>
							<dd>IP address the request was received from.</dd>
						<dt><b>User</b></dt>
							<dd>Username of the authenticated user.</dd>
						<dt><b>Method</b></dt>
							<dd>Request type (GET/POST/PATCH/DELETE).</dd>
						<dt><b>Resource</b></dt>
							<dd>URI provided in the request.</dd>
						<dt><b>Status</b></dt>
							<dd>response code.</dd>
					</dl>
				</paper-dialog-scrollable>
			</paper-dialog>
			<paper-dialog class="helpdialog" id="helpUsers">
				<app-toolbar>
					<paper-icon-button
							icon="my-icons:arrow-back"
							on-click="helpBack">
					</paper-icon-button>
					<h2>Users</h2>
				</app-toolbar>
				<paper-icon-item>
						<iron-icon icon="icons:assignment" slot="item-icon"></iron-icon>
					<h2>Users</h2>
				</paper-icon-item>
				<paper-dialog-scrollable>
					<p>To access the <b>Users</b> view first open the navigation drawer
					by selecting it's icon <iron-icon class="iconHelp" icon="menu"></iron-icon>
					on the left of the toolbar at top. Select the
					<iron-icon class="iconHelp" icon="icons:perm-identity"></iron-icon>
					<b>Users</b> view from the navigation menu.</p>
					<p><b>Users</b> are entities which may authenticate with an HTTP
					server within OCS for REST API or web user interface access. Each user
					requires a unique <b>Username</b> and <b>Password</b>. A language may
					also be chosen which will internationalize the web user interface.</p>
					<h3>Add a User</h3>
					<p>Select the floating action button
					<iron-icon class="iconHelp" icon="add"></iron-icon> at the bottom right
					of the view. An input dialog will appear. Enter the details for a new
					user then press the <b>SUBMIT</b> button. A dropdown selection provides
					a choice of available languages.</p>
					<h3>Update a User</h3>
					<p>Selecting a row in the Users view will open a dialog allowing
					updating of the <b>Password</b> or <b>Language</b>. Select the
					<b>UPDATE</b> button to submit.</p>
					<h3>Delete a User</h3>
					<p>Selecting a row in the Users view will open a dialog which
					includes a <b>DELETE</b> button.</p>
				</paper-dialog-scrollable>
			</paper-dialog>
			<paper-dialog class="helpdialog" id="helpFiltering">
				<app-toolbar>
					<paper-icon-button
						icon="my-icons:arrow-back"
						on-click="helpBack">
					</paper-icon-button>
					<h2>Filtering</h2>
				</app-toolbar>
				<paper-icon-item>
					<iron-icon icon="icons:assignment" slot="item-icon"></iron-icon>
					<h2>Filtering</h2>
				</paper-icon-item>
				<paper-dialog-scrollable>
					<p>Within a grid view (e.g. Subscribers, Clientrs, Logs) the
					displayed rows may be filtered by selecting column header(s)
					and entering match values.</p>
					<h3>Filter a View</h3>
					<p>In the example below the filter "AP2" is entered in the
					<b>NAS Identifier</b> column header of the <b>Clients</b> view
					resulting in only rows with a matching prefix appearing:</p>
					<img alt="screenshot" src="../images/filter.png" height="118" width="320">
				</paper-dialog-scrollable>
			</paper-dialog>
			<paper-dialog class="helpdialog" id="helpProduct">
				<app-toolbar>
					<paper-icon-button
						icon="my-icons:arrow-back"
						on-click="helpBack">
					</paper-icon-button>
					<h2>Product</h2>
				</app-toolbar>
				<paper-icon-item>
					<iron-icon icon="icons:assignment" slot="item-icon"></iron-icon>
					<h2>Product</h2>
				</paper-icon-item>
				<paper-dialog-scrollable>
					<p>To add <b>Product</b> first open the navigation drawer by selecting its icon
					<iron-icon class="iconHelp" icon="menu"></iron-icon> on the left of the toolbar
					at top. Select the <iron-icon class="iconHelp" icon="icons:store"></iron-icon>
					<b>Product</b> view from the navigation menu.</p><p>
					<h3>Add Product</h3>
					<p>Select the floating action button <iron-icon class="iconHelp" icon="add">
					</iron-icon> at the bottom right of the view. An <b>Add Product</b> dialog will
					appear.<b>Offering, Prices, Alterations</b> tabs are available in the
					<b>Add Product</b> dialog.Fill all details in <b>Alterations</b>, if required,
					first and press <b>Add</b> to add the list of available alterations. Fill all
					details in <b>Prices</b> tab and press <b>Add</b> to add the list of available
					prices. Finally fill all details in <b>Offering</b> tab and press <b>Submit</b>.</p>
					<h3>Validations</h3>
					<p><dl>
						<dt><b>Bytes</b></dt>
							<dd>When selected <b>Units</b> is bytes the <b>Unit Size</b> field user
							should be the number of bytes for the price <b>Amount</b>. Multipliers
							may be used: <b>m</b> for <b>Megabytes</b>, <b>g</b> for <b>Gigabytes</b>,
							<b>k</b> for <b>Kilobytes</b></dd>
						<dt><b>Seconds</b></dt>
							<dd>When selected <b>Units</b> is seconds the <b>Unit Size</b> field
							user should be the number of seconds for the price <b>Amount</b>. Multipliers
							may be used: <b>m</b> for <b>minutes</b>, <b>h</b> for <b>hours</b></dd>
						<dt><b>Cents</b></dt>
							<dd>When selected <b>Units</b> is cents the <b>Unit Size</b> field
							user should be the number of cents for the price <b>Amount</b>.</dd>
					</dl></p>
				</paper-dialog-scrollable>
			</paper-dialog>
			<paper-dialog class="helpdialog" id="aboutHelp">
				<app-toolbar>
					<h2>About</h2>
				</app-toolbar>
				<p>Online Charging System (OCS)<br>
				ocs-3.1.40<br>
				Copyright 2016 - 2021 SigScale Global Inc.<br>
				Apache License Version 2.0<br>
				<a target="_blank" href="http://www.sigscale.org">www.sigscale.org</a></p>
			</paper-dialog>
		`;
	}

	static get properties() {
		return {
			active: {
				type: Boolean,
				observer: '_dialog'
			}
		}
	}

	ready() {
		super.ready();
	}

	about(event) {
		var aboutOcs = document.querySelector('sig-app').shadowRoot.getElementById('ocsGetHelp');
		var aboutOcs1 = aboutOcs.shadowRoot.getElementById("helpDrop").close();
		var aboutObj = aboutOcs.shadowRoot.getElementById("aboutHelp");
		aboutObj.open();
	}

	helpId(event) {
		var helpOcs = document.querySelector('sig-app').shadowRoot.getElementById('ocsGetHelp');
		var helpOcs1 = helpOcs.shadowRoot.getElementById("helpDrop").close();
		var helpObj = helpOcs.shadowRoot.getElementById("helpMenu");
		helpObj.open();
	}

	addClientQue(event) {
		var clientOcs = document.querySelector('sig-app').shadowRoot.getElementById('ocsGetHelp');
		var clientOcs1 = clientOcs.shadowRoot.getElementById("helpMenu").close();
		var queObj = clientOcs.shadowRoot.getElementById("helpClients");
		queObj.open();
	}

	addSubQue(event) {
		var subOcs = document.querySelector('sig-app').shadowRoot.getElementById('ocsGetHelp');
		var subOcs1 = subOcs.shadowRoot.getElementById("helpMenu").close();
		var queObj = subOcs.shadowRoot.getElementById("helpSubscribers");
		queObj.open();
	}

	addUserQue(event) {
		var userOcs = document.querySelector('sig-app').shadowRoot.getElementById('ocsGetHelp');
		var userOcs1 = userOcs.shadowRoot.getElementById("helpMenu").close();
		var queObj = userOcs.shadowRoot.getElementById("helpUsers");
		queObj.open();
	}

	viewLogQue(event) {
		var logOcs = document.querySelector('sig-app').shadowRoot.getElementById('ocsGetHelp');
		var logOcs1 = logOcs.shadowRoot.getElementById("helpMenu").close();
		var queObj = logOcs.shadowRoot.getElementById("helpLogs");
		queObj.open();
	}

	toolQue(event) {
		var toolOcs = document.querySelector('sig-app').shadowRoot.getElementById('ocsGetHelp');
		var toolOcs1 = toolOcs.shadowRoot.getElementById("helpMenu").close();
		var queObj = toolOcs.shadowRoot.getElementById("helpToolbar");
		queObj.open();
	}

	filter(event) {
		var filterOcs = document.querySelector('sig-app').shadowRoot.getElementById('ocsGetHelp');
		var filterOcs1 = filterOcs.shadowRoot.getElementById("helpMenu").close();
		var queObj = filterOcs.shadowRoot.getElementById("helpFiltering");
		queObj.open();
	}

	product(event) {
		var proOcs = document.querySelector('sig-app').shadowRoot.getElementById('ocsGetHelp');
		var proOcs1 = proOcs.shadowRoot.getElementById("helpMenu").close();
		var queObj = proOcs.shadowRoot.getElementById("helpProduct");
		queObj.open();
	}

	helpBack(event) {
		var helpBackOcs = document.querySelector('sig-app').shadowRoot.getElementById('ocsGetHelp');
		var helpToolDiag= helpBackOcs.shadowRoot.getElementById("helpToolbar").close();
		var helpSubDiag= helpBackOcs.shadowRoot.getElementById("helpSubscribers").close();
		var helpClientDiag= helpBackOcs.shadowRoot.getElementById("helpClients").close();
		var helpUsersDiag= helpBackOcs.shadowRoot.getElementById("helpUsers").close();
		var helpLogsDiag= helpBackOcs.shadowRoot.getElementById("helpLogs").close();
		var helpFilterDiag= helpBackOcs.shadowRoot.getElementById("helpFiltering").close();
		var helpProductDiag= helpBackOcs.shadowRoot.getElementById("helpProduct").close();
		var helpMenuDiag= helpBackOcs.shadowRoot.getElementById("helpMenu").open();
	}

	_dialog(newValue) {
		var ocs = document.querySelector('sig-app');
		var help = ocs.shadowRoot.getElementById('ocsGetHelp');
		var dialog = help.shadowRoot.getElementById('helpDrop');
		var overFlow = ocs.shadowRoot.getElementById('overFlowIcon');
		if(newValue) {
			dialog.positionTarget = overFlow;
			dialog.open();
		} else {
			dialog.close();
		}
	}

	_collapseLogsContact(event) {
		var ocs = document.querySelector('sig-app').shadowRoot.getElementById('ocsGetHelp');
		var ocsContact = ocs.shadowRoot.getElementById('contact');
		if(ocsContact.opened == false) {
			ocsContact.show();
		} else {
			ocsContact.hide();
		}
	}
}

window.customElements.define('sig-help', ocsHelp);
