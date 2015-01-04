import QtQuick 2.0
import QtQuick.Controls 1.0
import QtQuick.Window 2.0
import HsQML.Canvas 1.0

Window {
    title: 'Context Controls';
    width: col.width; height: col.height;
    visible: true;

    Column {
        id: col;

        GroupBox {
            title: 'Control';

            Grid {
                columns: 2; width: col.width;
                columnSpacing: 5;

                Label {
                    text: 'Major Version';
                    font.bold: true;
                }
                TextField {
                    id: majorVersionField;
                }
                Label {
                    text: 'Minor Version';
                    font.bold: true;
                }
                TextField {
                    id: minorVersionField;
                }
                Label {
                    text: 'Context Type';
                    font.bold: true;
                }
                ComboBox {
                    id: contextTypeField;
                    model: ListModel {
                        ListElement { text: 'Type Unset'; }
                        ListElement { text: 'OpenGL';
                            type: OpenGLContextControl.OpenGL; }
                        ListElement { text: 'OpenGL ES';
                            type: OpenGLContextControl.OpenGLES; }
                    }
                }
                Label {
                    text: 'Context Profile';
                    font.bold: true;
                }
                ComboBox {
                    id: contextProfileField;
                    model: ListModel {
                        ListElement { text: 'Profile Unset'; }
                        ListElement { text: 'No Profile';
                            profile: OpenGLContextControl.NoProfile;}
                        ListElement { text: 'Core Profile';
                            profile: OpenGLContextControl.CoreProfile;}
                        ListElement { text: 'Compatibility Profile';
                            profile: OpenGLContextControl.CompatibilityProfile;}
                    }
                }
                Label {
                    text: 'Deprecated Functions';
                    font.bold: true;
                }
                ComboBox {
                    id: deprecatedFunctionsField;
                    model: ListModel {
                        ListElement { text: 'Unset'; }
                        ListElement { text: 'Enabled'; value: true; }
                        ListElement { text: 'Disabled'; value: false; }
                    }
                }
                Button {
                    text: 'Control Context';
                    onClicked: {
                        victimCtrl.when = false;
                        victimCtrl.majorVersion =
                            parseInt(majorVersionField.text);
                        victimCtrl.minorVersion =
                            parseInt(minorVersionField.text);
                        victimCtrl.contextType = contextTypeField.model.get(
                            contextTypeField.currentIndex).type;
                        victimCtrl.contextProfile =
                            contextProfileField.model.get(
                                contextProfileField.currentIndex).profile;
                        victimCtrl.deprecatedFunctions =
                            deprecatedFunctionsField.model.get(
                                deprecatedFunctionsField.currentIndex).value;
                        victimCtrl.when = true;
                        victim.visible = true;
                    }
                }
            }
        }
        GroupBox {
            title: 'Status';

            Grid {
                columns: 2; width: col.width;
                columnSpacing: 5;

                Label {
                    text: 'Vendor String';
                    font.bold: true;
                }
                Label {
                    text: vendor;
                }
                Label {
                    text: 'Renderer String';
                    font.bold: true;
                }
                Label {
                    text: renderer;
                }
                Label {
                    text: 'Version String';
                    font.bold: true;
                }
                Label {
                    text: version;
                }
                Label {
                    text: 'Major Version';
                    font.bold: true;
                }
                Label {
                    text: victimCtrl.majorVersion;
                }
                Label {
                    text: 'Minor Version';
                    font.bold: true;
                }
                Label {
                    text: victimCtrl.minorVersion;
                }
                Label {
                    text: 'Context Type';
                    font.bold: true;
                }
                Label {
                    text: ['Unknown','OpenGL','OpenGL ES'][
                        victimCtrl.contextType];
                }
                Label {
                    text: 'Context Profile';
                    font.bold: true;
                }
                Label {
                    text: ['None','Core','Compatability'][
                        victimCtrl.contextProfile];
                }
                Label {
                    text: 'Deprecated Functions';
                    font.bold: true;
                }
                Label {
                    text: victimCtrl.deprecatedFunctions;
                }
            }
        }
    }

    Window {
        id: victim;
        title: 'Victim Window';
        visible: true;

        HaskellCanvas {
            id: victimCanvas;
            anchors.fill: parent;
            delegate: glDelegate;
        }
        OpenGLContextControl {
            id: victimCtrl;
        }
    }
}
