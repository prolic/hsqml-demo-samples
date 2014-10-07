import QtQuick 2.0
import HsQML.Canvas 1.0

Rectangle {
    width: 500; height: 500;

    HaskellCanvas {
        x: 125; y: 125;
        width: 250; height: 250;
        delegate: myDelegate;
        model: t;

        property real t; // Can't animate model directly
        SequentialAnimation on t {
            loops: Animation.Infinite;
            NumberAnimation {
                from: 0; to: 1; duration: 1000;
            }
            NumberAnimation {
                from: 1; to: 0; duration: 1000;
            }
        }
    }
}
