function setLabel(slider) {
	var value = slider.value;
	document.getElementById('temp_value').textContent = value;
}


function updateEditorContent(editor, newContent) {
	var editor = ace.edit(editor);
	editor.getSession().setValue(newContent);
}


function parseReq() {
	var editor = ace.edit("logEditor");
	const editorContent = editor.getValue();

	const parsereq = {
			type: `parse`,
			case: document.getElementById("protocol_sel").value,
			doc: editorContent,
			model: document.getElementById("model_sel").value,
			mode: document.getElementById("prompt_mode").value,
			temperature: document.getElementById('temp_value').textContent
	};
	fetch(`${window.location.href}`, {
					method: "POST",
					credentials: "include",
					cache: "no-cache",
					headers: new Headers({
							"content-type": "application/json"
					}),
					body: JSON.stringify(parsereq)
			})
			.then(response => response.json())
			.then(data => {
					console.log(data.parseLogs)
					var editor = ace.edit("logEditor");
					editor.getSession().setValue(data.parseLogs);
					updateEditorContent(`sapicEditor`, data.parseResp);

			})
			.catch(error => {
					console.error('Error:', error);
					alert('Error: ' + error.message);
			});
}


function synthesisReq() {
	var editor = ace.edit("sapicEditor");
	const editorContent = editor.getValue();

	const synthesisReq = {
			type: `synthesis`,
			editorContent: editorContent,
			case: document.getElementById("protocol_sel").value
	};

	fetch(`${window.location.href}`, {
					method: "POST",
					credentials: "include",
					cache: "no-cache",
					headers: new Headers({
							"content-type": "application/json"
					}),
					body: JSON.stringify(synthesisReq)
			})
			.then(response => response.json())
			.then(data => {
					editor.getSession().setValue(data.compile);
			})
			.catch(error => {
					console.error('Error:', error);
					alert('Error: ' + error.message);
			});
}

function analysisReq() {
	var logEditor = ace.edit("logEditor");
	const editorContent = logEditor.getValue();

	var diagram = go.Diagram.fromDiv("diagramMSC");
	dataNodes = diagram.model.nodeDataArray;
	var IsGroup = dataNodes.filter(function(dataNode) {
			return dataNode.isGroup;
	});


	var data = {
			type: `analysis`,
			logs: editorContent
	};

	fetch(`${window.location.href}`, {
					method: "POST",
					credentials: "include",
					cache: "no-cache",
					headers: new Headers({
							"content-type": "application/json"
					}),
					body: JSON.stringify(data)
			})
			.then(response => response.json())
			.then(data => {
					updateEditorContent("logEditor", editorContent + data.report)
					diagram.model = go.Model.fromJson(data.msc);

			})
			.catch(error => {
					console.error('Error:', error);
					alert('Error: ' + error.message);
			});

}

function rewriteReq() {
	var logEditor = ace.edit("logEditor");
	const editorContent = logEditor.getValue();

	var data = {
			type: `rewrite`,
			logs: editorContent
	};

	fetch(`${window.location.href}`, {
					method: "POST",
					credentials: "include",
					cache: "no-cache",
					headers: new Headers({
							"content-type": "application/json"
					}),
					body: JSON.stringify(data)
			})
			.then(response => response.json())
			.then(data => {

					updateEditorContent(`sapicEditor`, data.rewriteResp);

			})
			.catch(error => {
					console.error('Error:', error);
					alert('Error: ' + error.message);
			});
}



function debug(message) {
	$('#debug').html(message);
}

function debug_obj(obj) {
	$('#debug').html(JSON.stringify(obj));
}


function loadMSC(name, mscJson) {

	// Since 2.2 you can also author concise templates with method chaining instead of GraphObject.make
	// For details, see https://gojs.net/latest/intro/buildingObjects.html
	const $$ = go.GraphObject.make;

	const myDiagram =
			$$(go.Diagram, name, // must be the ID or reference to an HTML DIV
					{
							allowCopy: false,
							linkingTool: $$(MessagingTool), // defined below
							"resizingTool.isGridSnapEnabled": true,
							draggingTool: $$(MessageDraggingTool), // defined below
							"draggingTool.gridSnapCellSize": new go.Size(1, MessageSpacing / 4),
							"draggingTool.isGridSnapEnabled": true,
							// automatically extend Lifelines as Activities are moved or resized
							"SelectionMoved": ensureLifelineHeights,
							"PartResized": ensureLifelineHeights,
							"undoManager.isEnabled": true
					});

	function ensureLifelineHeights(e) {
			// iterate over all Activities (ignore Groups)
			const arr = myDiagram.model.nodeDataArray;
			let max = -1;
			for (let i = 0; i < arr.length; i++) {
					const act = arr[i];
					if (act.isGroup) continue;
					max = Math.max(max, act.start + act.duration);
			}
			if (max > 0) {
					// now iterate over only Groups
					for (let i = 0; i < arr.length; i++) {
							const gr = arr[i];
							if (!gr.isGroup) continue;
							if (max > gr.duration) { // this only extends, never shrinks
									myDiagram.model.setDataProperty(gr, "duration", max);
							}
					}
			}
	}
	// when the document is modified, add a "*" to the title and enable the "Save" button

	function computeActivityLocation(act) {
			const groupdata = myDiagram.model.findNodeDataForKey(act.group);
			if (groupdata === null) return new go.Point();
			// get location of Lifeline's starting point
			const grouploc = go.Point.parse(groupdata.loc);
			return new go.Point(grouploc.x, convertTimeToY(act.start) - ActivityStart);
	}
	myDiagram.addDiagramListener("Modified", e => {
			const button = document.getElementById("SaveButton");
			if (button) button.disabled = !myDiagram.isModified;
			const idx = document.title.indexOf("*");
			if (myDiagram.isModified) {
					if (idx < 0) document.title += "*";
			} else {
					if (idx >= 0) document.title = document.title.slice(0, idx);
			}
	});

	// define the Lifeline Node template.
	myDiagram.groupTemplate =
			$$(go.Group, "Vertical", {
							locationSpot: go.Spot.Bottom,
							locationObjectName: "HEADER",
							minLocation: new go.Point(0, 0),
							maxLocation: new go.Point(9999, 0),
							selectionObjectName: "HEADER"
					},
					new go.Binding("location", "loc", go.Point.parse).makeTwoWay(go.Point.stringify),
					$$(go.Panel, "Auto", {
									name: "HEADER"
							},
							$$(go.Shape, "Rectangle", {
									fill: $$(go.Brush, "Linear", {
											0: "#bbdefb",
											1: go.Brush.darkenBy("#bbdefb", 0.1)
									}),
									width: 80,
									stroke: null
							}),
							$$(go.TextBlock, {
											margin: 5,
											font: "400 10pt Source Sans Pro, sans-serif"
									},
									new go.Binding("text", "text"))
					),
					$$(go.Shape, {
									figure: "LineV",
									fill: null,
									stroke: "gray",
									// strokeDashArray: [3, 3],
									width: 1,
									alignment: go.Spot.Center,
									portId: "",
									fromLinkable: true,
									fromLinkableDuplicates: true,
									toLinkable: true,
									toLinkableDuplicates: true,
									cursor: "pointer"
							},
							new go.Binding("height", "duration", computeLifelineHeight)),
					$$(go.Panel, "Auto", {
									name: "HEADER"
							},
							$$(go.Shape, "Rectangle", {
									fill: "black",
									width: 60,
									height: 8,
									stroke: null,
							}),
					)
			);

	// define the Activity Node template
	myDiagram.nodeTemplate =
			$$(go.Node, {
							locationSpot: go.Spot.Top,
							locationObjectName: "SHAPE",
							minLocation: new go.Point(NaN, LinePrefix - ActivityStart),
							maxLocation: new go.Point(NaN, 19999),
							selectionObjectName: "SHAPE",
							resizable: true,
							resizeObjectName: "SHAPE",
							resizeAdornmentTemplate: $$(go.Adornment, "Spot",
									$$(go.Placeholder),
									$$(go.Shape, // only a bottom resize handle
											{
													alignment: go.Spot.Bottom,
													cursor: "col-resize",
													desiredSize: new go.Size(6, 6),
													fill: "yellow"
											})
							)
					},
					new go.Binding("location", "", computeActivityLocation).makeTwoWay(backComputeActivityLocation),
					$$(go.Panel, "Auto",
							$$(go.Shape, "Rectangle", // This Shape acts as the border
									{
											fill: "white",
											stroke: "blue", // Border color
											strokeWidth: 1, // Reduced border thickness
											strokeDashArray: [4, 2] // Adjusted dash pattern for a finer appearance
									},
									new go.Binding("height", "duration", computeActivityHeight).makeTwoWay(backComputeActivityHeight)),
							$$(go.TextBlock, {
											margin: 3, // Ensure some spacing between text and border
											// Other TextBlock properties here
											background: "white",
											font: "8pt  Source Sans Pro",
									},
									new go.Binding("text", "text"),
							)
					)

			);

	// define the Message Link template.
	myDiagram.linkTemplate =
			$$(MessageLink, // defined below
					{
							selectionAdorned: true,
							curviness: 0
					},
					$$(go.Shape, "Rectangle", {
							stroke: "#DC143C"
					}),
					$$(go.Shape, {
							toArrow: "Standard",
							stroke: "#DC143C",
							fill: "#DC143C"
					}),
					$$(go.TextBlock, {
									font: "400 9pt Source Sans Pro, sans-serif",
									segmentIndex: 0,
									segmentOffset: new go.Point(NaN, NaN),
									isMultiline: false,
									editable: true
							},
							new go.Binding("text", "text").makeTwoWay())
			);

	// create the graph by reading the JSON data saved in "mySavedModel" textarea element
	myDiagram.model = go.Model.fromJson(mscJson);
}


// some parameters
const LinePrefix = 18; // vertical starting point in document for all Messages and Activations
const LineSuffix = 20; // vertical length beyond the last message time
const MessageSpacing = 22; // vertical distance between Messages at different steps
const ActivityWidth = 10; // width of each vertical activity bar
const ActivityStart = 5; // height before start message time
const ActivityEnd = 5; // height beyond end message time

function computeLifelineHeight(duration) {
	return LinePrefix + duration * MessageSpacing + LineSuffix;
}

function backComputeActivityLocation(loc, act) {
	myDiagram.model.setDataProperty(act, "start", convertYToTime(loc.y + ActivityStart));
}

function computeActivityHeight(duration) {
	return ActivityStart + duration * MessageSpacing + ActivityEnd;
}

function backComputeActivityHeight(height) {
	return (height - ActivityStart - ActivityEnd) / MessageSpacing;
}

// time is just an abstract small non-negative integer
// here we map between an abstract time and a vertical position
function convertTimeToY(t) {
	return t * MessageSpacing + LinePrefix;
}

function convertYToTime(y) {
	return (y - LinePrefix) / MessageSpacing;
}


// a custom routed Link
class MessageLink extends go.Link {
	constructor() {
			super();
			this.time = 0; // use this "time" value when this is the temporaryLink
	}

	getLinkPoint(node, port, spot, from, ortho, othernode, otherport) {
			const p = port.getDocumentPoint(go.Spot.Center);
			const r = port.getDocumentBounds();
			const op = otherport.getDocumentPoint(go.Spot.Center);

			const data = this.data;
			const time = data !== null ? data.time : this.time; // if not bound, assume this has its own "time" property

			const aw = this.findActivityWidth(node, time);
			const x = (op.x > p.x ? p.x + aw / 2 : p.x - aw / 2);
			const y = convertTimeToY(time);
			return new go.Point(x, y);
	}

	findActivityWidth(node, time) {
			let aw = ActivityWidth;
			if (node instanceof go.Group) {
					// see if there is an Activity Node at this point -- if not, connect the link directly with the Group's lifeline
					if (!node.memberParts.any(mem => {
									const act = mem.data;
									return (act !== null && act.start <= time && time <= act.start + act.duration);
							})) {
							aw = 0;
					}
			}
			return aw;
	}

	getLinkDirection(node, port, linkpoint, spot, from, ortho, othernode, otherport) {
			const p = port.getDocumentPoint(go.Spot.Center);
			const op = otherport.getDocumentPoint(go.Spot.Center);
			const right = op.x > p.x;
			return right ? 0 : 180;
	}

	computePoints() {
			if (this.fromNode === this.toNode) { // also handle a reflexive link as a simple orthogonal loop
					const data = this.data;
					const time = data !== null ? data.time : this.time; // if not bound, assume this has its own "time" property
					const p = this.fromNode.port.getDocumentPoint(go.Spot.Center);
					const aw = this.findActivityWidth(this.fromNode, time);

					const x = p.x + aw / 2;
					const y = convertTimeToY(time);
					this.clearPoints();
					this.addPoint(new go.Point(x, y));
					this.addPoint(new go.Point(x + 50, y));
					this.addPoint(new go.Point(x + 50, y + 5));
					this.addPoint(new go.Point(x, y + 5));
					return true;
			} else {
					return super.computePoints();
			}
	}
}
// end MessageLink


class MessagingTool extends go.LinkingTool {
	constructor() {
			super();

			// Since 2.2 you can also author concise templates with method chaining instead of GraphObject.make
			// For details, see https://gojs.net/latest/intro/buildingObjects.html
			const $$ = go.GraphObject.make;
			this.temporaryLink =
					$$(MessageLink,
							$$(go.Shape, "Rectangle", {
									stroke: "magenta",
									strokeWidth: 2
							}),
							$$(go.Shape, {
									toArrow: "OpenTriangle",
									stroke: "magenta"
							}));
	}

	doActivate() {
			super.doActivate();
			const time = convertYToTime(this.diagram.firstInput.documentPoint.y);
			this.temporaryLink.time = Math.ceil(time); // round up to an integer value
	}

	insertLink(fromnode, fromport, tonode, toport) {
			const newlink = super.insertLink(fromnode, fromport, tonode, toport);
			if (newlink !== null) {
					const model = this.diagram.model;
					// specify the time of the message
					const start = this.temporaryLink.time;
					const duration = 1;
					newlink.data.time = start;
					model.setDataProperty(newlink.data, "text", "msg");
					// and create a new Activity node data in the "to" group data
					const newact = {
							group: newlink.data.to,
							start: start,
							duration: duration
					};
					model.addNodeData(newact);
					// now make sure all Lifelines are long enough
					ensureLifelineHeights();
			}
			return newlink;
	}
}
// end MessagingTool


// A custom DraggingTool that supports dragging any number of MessageLinks up and down --
// changing their data.time value.
class MessageDraggingTool extends go.DraggingTool {
	// override the standard behavior to include all selected Links,
	// even if not connected with any selected Nodes
	computeEffectiveCollection(parts, options) {
			const result = super.computeEffectiveCollection(parts, options);
			// add a dummy Node so that the user can select only Links and move them all
			result.add(new go.Node(), new go.DraggingInfo(new go.Point()));
			// normally this method removes any links not connected to selected nodes;
			// we have to add them back so that they are included in the "parts" argument to moveParts
			parts.each(part => {
					if (part instanceof go.Link) {
							result.add(part, new go.DraggingInfo(part.getPoint(0).copy()));
					}
			})
			return result;
	}

	// override to allow dragging when the selection only includes Links
	mayMove() {
			return !this.diagram.isReadOnly && this.diagram.allowMove;
	}

	// override to move Links (which are all assumed to be MessageLinks) by
	// updating their Link.data.time property so that their link routes will
	// have the correct vertical position
	moveParts(parts, offset, check) {
			super.moveParts(parts, offset, check);
			const it = parts.iterator;
			while (it.next()) {
					if (it.key instanceof go.Link) {
							const link = it.key;
							const startY = it.value.point.y; // DraggingInfo.point.y
							let y = startY + offset.y; // determine new Y coordinate value for this link
							const cellY = this.gridSnapCellSize.height;
							y = Math.round(y / cellY) * cellY; // snap to multiple of gridSnapCellSize.height
							const t = Math.max(0, convertYToTime(y));
							link.diagram.model.set(link.data, "time", t);
							link.invalidateRoute();
					}
			}
	}
}


data = {
	"class": "go.GraphLinksModel",
	"nodeDataArray": [{
					"key": "Fred",
					"text": "Fred",
					"isGroup": true,
					"loc": "0 0",
					"duration": 7
			},
			{
					"key": "Bob",
					"text": "Bob",
					"isGroup": true,
					"loc": "100 0",
					"duration": 7
			},
			{
					"key": "Hank",
					"text": "Hank",
					"isGroup": true,
					"loc": "200 0",
					"duration": 7
			},
	],
	"linkDataArray": [{
					"from": "Fred",
					"to": "Bob",
					"text": "message1",
					"time": 1
			},
			{
					"from": "Bob",
					"to": "Hank",
					"text": "message2",
					"time": 2
			},
			{
					"from": "Bob",
					"to": "Fred",
					"text": "message3",
					"time": 3
			},
			{
					"from": "Hank",
					"to": "Bob",
					"text": "message4",
					"time": 5
			},
			{
					"from": "Bob",
					"to": "Fred",
					"text": "message5",
					"time": 6
			},
	]
};

window.addEventListener('DOMContentLoaded', loadMSC('diagramMSC', data));

function hl(line, color) {
	$(`#sapic`).find("#True-" + line).attr('style', 'background-color:' + color);
}

$(document).ready(function() {
	$(`#protocol_sel`).change(function() {
			var selectedOption = $(this).val();
			$.ajax({
					url: '/get-loadtext',
					type: 'POST',
					data: {
							option: selectedOption
					},
					success: function(response) {
							updateEditorContent(`logEditor`, response.content);

					}
			});
	});
});

document.addEventListener('DOMContentLoaded', function() {
	const highlightContainer = document.querySelector('#sapic');
	highlightContainer.addEventListener('mouseenter', function(event) {
			if (event.target.matches('span[id^="True-"]')) {
					const lineNumber = event.target.id.split('-')[1];
					hl(lineNumber, '#61eb76');
					console.log('Line number: ' + lineNumber);
			}
	}, true);

	highlightContainer.addEventListener('mouseleave', function(event) {
			if (event.target.matches('span[id^="True-"]')) {
					const lineNumber = event.target.id.split('-')[1];
					hl(lineNumber, 'transparent');
			}
	}, true);


	var temperatureSlider = document.getElementById('temperature');
	setLabel(temperatureSlider);

});