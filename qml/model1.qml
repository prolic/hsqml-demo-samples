import QtQuick 2.0
import QtQuick.Controls 1.0
import QtQuick.Layouts 1.0
import HsQML.Model 1.0

ColumnLayout {
    spacing: 1;

    Text {
        Layout.fillWidth: true;
        text: 'Step 1) Enter a valid JSON array of values.';
    }
    TextField {
        id: input;
        Layout.fillWidth: true;
        text: '[]';

        property var json : null;
        onTextChanged: {
            try {
                json = JSON.parse(text);
                if (!(json instanceof Array)) {
                    json = null;
                }
            }
            catch (e) {
                json = null;
            }
        }
    }

    Text {
        Layout.fillWidth: true;
        text: 'Step 2) Select the mode for calculating model changes.'
    }
    ComboBox {
        id: modeSelector;
        Layout.fillWidth: true;
        model: ListModel {
            ListElement { text: 'By Reset'; mode: AutoListModel.ByReset;}
            ListElement { text: 'By Index'; mode: AutoListModel.ByIndex;}
            ListElement { text: 'By Key'; mode: AutoListModel.ByKey;}
            ListElement {
                text: 'By Key (No Reorder)';
                mode: AutoListModel.ByKeyNoReorder;
            }
        }
    }

    Text {
        Layout.fillWidth: true;
        text: 'Step 3) Click the button to change the model.'
    }
    Button {
        Layout.fillWidth: true;
        text: 'Update Model';
        enabled: input.json != null;
        onClicked: autoModel.source = input.json;
    }

    Rectangle {
        Layout.fillWidth: true;
        Layout.fillHeight: true;
        Layout.preferredWidth: 300;
        Layout.minimumHeight: 30;
        color: 'lightblue';

        ListView {
            id: view;
            anchors.fill: parent;
            orientation: ListView.Horizontal;
            model: AutoListModel {
                id: autoModel;
                mode: modeSelector.model.get(modeSelector.currentIndex).mode;
            }
            delegate: Item {
                width: 30; height: 30;
                clip: true;

                Rectangle {
                    anchors.fill: parent;
                    border.width: 1;
                    border.color: 'white';
                    color: 'blue';
                    opacity: 0.5;
                }

                Text {
                    anchors.centerIn: parent;
                    font.pixelSize: 25;
                    text: String(modelData);
                }
            }
            displaced: Transition {
                NumberAnimation { properties: "x,y"; duration: 1000; }
                NumberAnimation { properties: "scale"; duration: 1000; to: 1; }
            }
            move: Transition {
                NumberAnimation { properties: "x,y"; duration: 1000; }
                NumberAnimation { properties: "scale"; duration: 1000; to: 1; }
            }
            add: Transition {
                NumberAnimation {
                    properties: "scale"; duration: 1000; from: 0; to: 1;
                }
            }
            remove: Transition {
                NumberAnimation {
                    properties: "scale"; duration: 1000; to: 0;
                }
            }
        }
    }
}
