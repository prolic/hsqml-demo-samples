import QtQuick 2.0
import QtQuick.Controls 1.0
import HsQML.Canvas 1.0

Column {
    width: 500;

    Rectangle {
        width: 500; height: 500;

        Rectangle {
            anchors.top: parent.top; anchors.left: parent.left;
            anchors.topMargin: 50; anchors.leftMargin: 50;
            width: 100; height: 100; color: "black";
            Text {
                anchors.centerIn: parent; font.pixelSize: 30;
                text: "Below"; color: "white";
            }
        }

        HaskellCanvas {
            id: canvas;
            x: 125; y: 125;
            width: 250; height: 250;
            delegate: myDelegate;
            model: t;

            property real t; // Can't animate model directly
            SequentialAnimation on t {
                paused: modelAnimationOff.checked;
                loops: Animation.Infinite;
                NumberAnimation {
                    from: 0; to: 1; duration: 2500;
                }
                NumberAnimation {
                    from: 1; to: 0; duration: 2500;
                }
            }

            PathAnimation {
                target: canvas;
                running: true;
                paused: sgAnimationOff.checked;
                loops: Animation.Infinite;
                duration: 5000;
                orientation: PathAnimation.Fixed;
                anchorPoint: Qt.point(canvas.width/2, canvas.height/2);
                path: Path {
                    startX: 250; startY: 250;
                    PathLine { x: 200; y: 200; }
                    PathLine { x: 200; y: 300; }
                    PathLine { x: 300; y: 300; }
                    PathLine { x: 300; y: 200; }
                    PathLine { x: 250; y: 250; }
                }
            }
        }

        Rectangle {
            anchors.top: parent.top; anchors.right: parent.right;
            anchors.topMargin: 50; anchors.rightMargin: 50;
            width: 100; height: 100; color: "black";
            Text {
                anchors.centerIn: parent; font.pixelSize: 30;
                text: "Above"; color: "white";
            }
        }
    }

    GroupBox {
        title: "Display Mode";

        Row {
            ExclusiveGroup {id: displayModeGrp;}
            RadioButton {
                text: "Above";
                exclusiveGroup: displayModeGrp;
                onCheckedChanged: {
                    if (checked) canvas.displayMode = HaskellCanvas.Above;}
            }
            RadioButton {
                text: "Inline";
                exclusiveGroup: displayModeGrp;
                checked: true;
                onCheckedChanged: {
                    if (checked) canvas.displayMode = HaskellCanvas.Inline;}
            }
            RadioButton {
                text: "Below";
                exclusiveGroup: displayModeGrp;
                onCheckedChanged: {
                    if (checked) canvas.displayMode = HaskellCanvas.Below;}
            }
        }
    }

    GroupBox {
        title: "Model Animation (Colour)";

        Row {
            ExclusiveGroup {id: modelAnimationGrp;}
            RadioButton {
                text: "On";
                exclusiveGroup: modelAnimationGrp;
                checked: true;
            }
            RadioButton {
                id: modelAnimationOff;
                text: "Off";
                exclusiveGroup: modelAnimationGrp;
            }
        }
    }

    GroupBox {
        title: "Scenegraph Animation (Position)";

        Row {
            ExclusiveGroup {id: sgAnimationGrp;}
            RadioButton {
                text: "On";
                exclusiveGroup: sgAnimationGrp;
                checked: true;
            }
            RadioButton {
                id: sgAnimationOff;
                text: "Off";
                exclusiveGroup: sgAnimationGrp;
            }
        }
    }
}
