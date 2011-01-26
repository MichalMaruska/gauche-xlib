/* -*- Mode: C; tab-width: 3; indent-tabs-mode: nil; c-basic-offset: 3 -*- */
#ifndef XLIB_LIB_H
#define XLIB_LIB_H


#include <gauche.h>
#include <gauche/class.h>
#include <gauche/extend.h>	/*  why ?*/
#include <gauche/uvector.h>


/* "#undef XLIB_ILLEGAL_ACCESS" */
/* #define XLIB_ILLEGAL_ACCESS 1 */

#include  <X11/Xlib.h>
#include  <X11/Xutil.h>

#include  <X11/extensions/XKB.h>
#include  <X11/extensions/XKBstr.h>
// #include  <X11/extensions/XTest.h>
#include  <X11/extensions/XKBgeom.h>
#include <X11/extensions/XInput.h>

#include  <X11/XKBlib.h>
// #include  <X11/extensions/dpms.h>

#include  <X11/extensions/fork.h>


#define CHECK_KEYCODE_IN(desc,keycode) \
  if ((keycode > desc->max_key_code) || (keycode < desc->min_key_code))\
    Scm_Error("keycode index out of bounds", keycode, desc->min_key_code, desc->max_key_code);



/* basic types: should be #included */
#define CONST_CHAR_PTR(string)   ((const char*)Scm_GetStringConst(SCM_STRING(string)))
extern  ScmObj Scm_pg_charPtr_Box(char *string);



/* fixme:  This should go into a separate *.h file! */
typedef struct ScmPointerRec {
  SCM_HEADER;
  void* pointer;
} ScmPointer;

SCM_CLASS_DECL( Scm_PointerClass );
#define SCM_CLASS_POINTER (&Scm_PointerClass)
#define SCM_POINTER(obj) ((ScmPointer*)(obj))
#define SCM_POINTER_P(obj) SCM_XTYPEP( obj , SCM_CLASS_POINTER )
#define SCM_POINTER_VALUE(obj) (SCM_POINTER(obj))->pointer

extern ScmObj Scm_MakePointer( void* pointer );

/* fixme! */
#define SCM_XGCVALUES_VALUE(obj) ((XGCValues*) SCM_POINTER_VALUE(obj))
#define SCM_X_GC_VALUE(obj) ((GC) SCM_POINTER_VALUE(obj))


int xlib_keyval_to_unicode (int keyval);
ScmObj XTime_to_ScmTime(Time t);
int xkb_apply_event_to_state (XkbStateNotifyEvent* ev, XkbStateRec* s);


extern int error_handler (Display *dpy, XErrorEvent *ev);

extern ScmObj new_x_dpy(Display* dpy);


SCM_CLASS_DECL(ScmXdpy_class);         /* handle*/

typedef struct ScmXKBDesc ScmXkbDesc;

typedef struct ScmXlibRec {
   SCM_HEADER;
   Display*  dpy;              /* NULL if closed */
   ScmXkbDesc* xkb_desc;
} ScmXDpy;

#define SCM_CLASS_X_DPY       (&ScmXdpy_class)
#define SCM_X_DPY(obj)        (((ScmXDpy*) obj)->dpy)
#define SCM_X_DPY_P(obj)       (Scm_TypeP(obj, SCM_CLASS_X_DPY) && (SCM_X_DPY(obj)))

void scm_xdpy_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx);


/*   macros           for both
 *   types                 
 *
 *   SCM_CLASS_DECL(ScmDB_class);    for the .c !!!
 *   
 *   export of functions from  .c    for .stub
 *   
 *  */




#define SCM_Time(x)   (SCM_TIME(x)->sec * 1000 +  SCM_TIME(x)->nsec/1000)

#define SCM_BYTE16_P(obj)  (SCM_INTP(obj) && (SCM_INT_VALUE(obj) &&( ~0xffff) == 0))
#define SCM_BYTE16(obj)  SCM_INT_VALUE(obj)



/***  Events  */

SCM_CLASS_DECL(ScmXEvent_class);         /*handle */

typedef struct ScmXEventRec {
    SCM_HEADER;
    XEvent*  event;              /* NULL if closed */
/*   struct ScmStringRec *name; */
 } ScmXEvent;

#define SCM_CLASS_X_EVENT       (&ScmXEvent_class)
#define SCM_X_EVENT(obj)        (((ScmXEvent*) obj)->event)
#define SCM_X_EVENT_P(obj)       Scm_TypeP(obj, SCM_CLASS_X_EVENT)



extern ScmObj new_x_event(XEvent* e);



typedef struct ScmXKeyEventRec {
    SCM_HEADER;
    XKeyEvent*  event;              /* NULL if closed */
    struct ScmStringRec *name;
 } ScmXKeyEvent;


SCM_CLASS_DECL(ScmXKeyEvent_class);      /*   ; handle */
#define SCM_CLASS_XKEY_EVENT       (&ScmXKeyEvent_class)
#define SCM_XKEY_EVENT(obj)        (((ScmXKeyEvent*) obj)->event)
#define SCM_XKEY_EVENT_P(obj)       Scm_TypeP(obj, SCM_CLASS_XKEY_EVENT)



extern ScmObj new_xkb_event(XkbEvent* event);
extern ScmObj new_xkey_event(XKeyEvent* e);

extern ScmObj new_xkb_msg_event(XkbActionMessageEvent* event);




/**** xkb events  */

SCM_CLASS_DECL(ScmXkbEvent_class);

typedef struct ScmXKBEventRec {
    SCM_HEADER;
    XkbEvent*  event;              /* NULL if closed */
 } ScmXkbEvent;

#define SCM_CLASS_XKB_EVENT       (&ScmXkbEvent_class)
#define SCM_XKB_EVENT(obj)        (((ScmXkbEvent*) obj)->event)
#define SCM_XKB_EVENT_P(obj)       Scm_TypeP(obj, SCM_CLASS_XKB_EVENT)


/*** fork */
SCM_CLASS_DECL(ScmXkbMessageEvent_class);

typedef struct ScmXKBMsgRec {
    SCM_HEADER;
    XkbActionMessageEvent*  event;              /* NULL if closed */
 } ScmXkbMsgEvent;

#define SCM_CLASS_XKB_MSG_EVENT       (&ScmXkbMessageEvent_class)
#define SCM_XKB_MSG_EVENT(obj)        (((ScmXkbMsgEvent*) obj)->event)
#define SCM_XKB_MSG_EVENT_P(obj)       Scm_TypeP(obj, SCM_CLASS_XKB_MSG_EVENT)


typedef struct Scmarchived_eventRec {
    SCM_HEADER;
    archived_event *data;
} Scmarchived_event;

SCM_CLASS_DECL(Scm_archived_event_Class);
#define SCM_CLASS_ARCHIVED_KEYEVENT     (&Scm_archived_event_Class)
#define SCM_ARCHIVED_KEYEVENT(obj)      ((archived_event*) (((Scmarchived_event*) obj)->data))
#define SCM_ARCHIVED_KEYEVENT_P(obj)    (Scm_TypeP(obj, SCM_CLASS_ARCHIVED_EVENT))
extern ScmObj Scm_Make_archived_event(archived_event *data);

/*** xkb state ***/

typedef struct ScmXkbStateRecRec {
    SCM_HEADER;
    XkbStateRec *data;
} ScmXkbStateRec;

SCM_CLASS_DECL(Scm_XkbStateRec_Class);
#define SCM_CLASS_XKBSTATEREC     (&Scm_XkbStateRec_Class)
#define SCM_XKB_STATE_P(obj)    (Scm_TypeP(obj, SCM_CLASS_XKBSTATEREC))
#define SCM_XKB_STATE(obj)      ((((ScmXkbStateRec*) obj)->data))
extern ScmObj Scm_Make_XkbStateRec(XkbStateRec *data);


/**** xkb  controls */

SCM_CLASS_DECL(ScmXkbControls_class); 

typedef struct ScmXKBControls {
    SCM_HEADER;
    /*do i need a pointer to the xkb-desc ? for GC*/
    ScmObj xkb_desc;
    XkbControlsPtr  controls;
 } ScmXkbControls;

#define SCM_CLASS_XKB_CONTROLS       (&ScmXkbControls_class)
#define SCM_XKB_CONTROLS(obj)        (((ScmXkbControls*) obj)->controls)
#define SCM_XKB_CONTROLS_P(obj)       Scm_TypeP(obj, SCM_CLASS_XKB_CONTROLS)

extern ScmObj new_xkb_controls(XkbControlsPtr controls);




/**** names  */
SCM_CLASS_DECL(ScmXkbNames_class); 


typedef struct ScmXKBNames {
    SCM_HEADER;
    /*do i need a pointer to the xkb-desc ? for GC*/
    ScmObj xkb_desc;
    XkbNamesPtr  names;
 } ScmXkbNames;

#define SCM_CLASS_XKB_NAMES       (&ScmXkbNames_class)
#define SCM_XKB_NAMES(obj)        (((ScmXkbNames*) obj)->names)
#define SCM_XKB_NAMES_P(obj)       Scm_TypeP(obj, SCM_CLASS_XKB_NAMES)

extern ScmObj new_xkb_names(XkbNamesPtr names);




SCM_CLASS_DECL(ScmXkbDesc_class);

/* ahead: */
typedef struct ScmXkbGeometry *ScmXkbGeometryPtr; /* fixme! */

typedef struct ScmXKBDesc {
   SCM_HEADER;
   XkbDescPtr  desc;
   ScmXDpy* scm_display;
   int verbose;
   ScmXkbGeometryPtr scm_geometry;
} ScmXKBDesc;

#define SCM_CLASS_XKB_DESC       (&ScmXkbDesc_class)
#define SCM_XKB_DESC(obj)        (((ScmXkbDesc*) obj)->desc)
#define SCM_XKB_DESC_P(obj)       Scm_TypeP(obj, SCM_CLASS_XKB_DESC)


/* mmc: */
extern ScmObj new_xkb_desc(XkbDescPtr desc, ScmXDpy*);




/* client map: */
SCM_CLASS_DECL(ScmXkbClient_Map_class);

typedef struct ScmXkbClient_Map {
   SCM_HEADER;
   XkbClientMapPtr client_map;
} ScmXkbClient_Map;

#define SCM_CLASS_XKB_CLIENT_MAP       (&ScmXkbClient_Map_class)
/* fixme: wrong! */
#define SCM_XKB_CLIENT_MAP(obj)        (((ScmXkbClient_Map*) obj)->client_map)
#define SCM_XKB_CLIENT_MAP_P(obj)      Scm_TypeP(obj, SCM_CLASS_XKB_CLIENT_MAP)

extern ScmObj new_xkb_client_map(XkbClientMapPtr map);



/* type  */

SCM_CLASS_DECL(ScmXkbType_class);

typedef struct ScmXkbType {
   SCM_HEADER;
   XkbKeyTypePtr type;
} ScmXkbType;

#define SCM_CLASS_XKB_TYPE       (&ScmXkbType_class)
#define SCM_XKB_TYPE(obj)        (((ScmXkbType*) obj)->type)
#define SCM_XKB_TYPE_P(obj)       Scm_TypeP(obj, SCM_CLASS_XKB_TYPE)


extern ScmObj new_xkb_type(XkbKeyTypePtr map);

extern int XkbKeyTypeIndex(XkbDescPtr desc, KeyCode keycode, int group);



/*** key -> types info */
SCM_CLASS_DECL(ScmXkbKey_Node_class);

typedef struct ScmXkbKey_Node {
   SCM_HEADER;
   XkbSymMapPtr key_node;
} ScmXkbKey_Node;

#define SCM_CLASS_XKB_KEY_NODE       (&ScmXkbKey_Node_class)
#define SCM_XKB_KEY_NODE(obj)        (((ScmXkbKey_Node*) obj)->key_node)
#define SCM_XKB_KEY_NODE_P(obj)       Scm_TypeP(obj, SCM_CLASS_XKB_KEY_NODE)


extern ScmObj new_xkb_node(XkbSymMapPtr node);




/*** server map */
SCM_CLASS_DECL(ScmXkbServer_Map_class);

typedef struct ScmXkbServer_Map {
   SCM_HEADER;
   XkbServerMapPtr server_map;
} ScmXkbServer_Map;

#define SCM_CLASS_XKB_SERVER_MAP       (&ScmXkbServer_Map_class)
#define SCM_XKB_SERVER_MAP(obj)        (((ScmXkbServer_Map*) obj)->server_map)
#define SCM_XKB_SERVER_MAP_P(obj)      Scm_TypeP(obj, SCM_CLASS_XKB_SERVER_MAP)

extern ScmObj new_xkb_server_map(XkbServerMapPtr map);

/* mapping  modifiers->lever  on a type */

SCM_CLASS_DECL(ScmXkbMapping_class);

typedef struct ScmXkbMapping {
   SCM_HEADER;
   XkbKTMapEntryPtr mapping;
} ScmXkbMapping;

#define SCM_CLASS_XKB_MAPPING       (&ScmXkbMapping_class)
#define SCM_XKB_MAPPING(obj)        (((ScmXkbMapping*) obj)->mapping)
#define SCM_XKB_MAPPING_P(obj)       Scm_TypeP(obj, SCM_CLASS_XKB_MAPPING)


extern ScmObj new_xkb_mapping(XkbKTMapEntryPtr map);


/*** Actions  */
typedef struct ScmXkbModActionRec {
    SCM_HEADER;
    XkbModAction *data;
} ScmXkbModAction;

SCM_CLASS_DECL(Scm_XkbModActionClass);
#define SCM_CLASS_XKB_MOD_ACTION     (&Scm_XkbModActionClass)
#define SCM_XKB_MOD_ACTION(obj)      (((ScmXkbModAction*) obj)->data)
#define SCM_XKB_MOD_ACTION_P(obj)    (Scm_TypeP(obj, SCM_CLASS_XKB_MOD_ACTION))

/* 
   extern ScmObj Scm_MakeXkbModAction(XkbModAction *data);
*/

extern ScmObj new_xkb_mod_action(XkbModAction* a);



typedef struct ScmXkbGroupActionRec {
    SCM_HEADER;
    XkbGroupAction *data;
} ScmXkbGroupAction;

SCM_CLASS_DECL(Scm_XkbGroupActionClass);
#define SCM_CLASS_XKB_GROUP_ACTION     (&Scm_XkbGroupActionClass)
#define SCM_XKB_GROUP_ACTION(obj)      (((ScmXkbGroupAction*) obj)->data)
#define SCM_XKB_GROUP_ACTION_P(obj)    (Scm_TypeP(obj, SCM_CLASS_XKB_GROUP_ACTION))
extern ScmObj Scm_MakeXkbGroupAction(XkbGroupAction *data);


extern ScmObj Scm_MakeXkbAction(XkbAnyAction *a);









/*** geometry */
SCM_CLASS_DECL(ScmXkbGeometry_class);

typedef struct ScmXkbGeometry {
   SCM_HEADER;
   XkbGeometryPtr geometry;
   ScmXkbDesc* desc;             /* back */
}ScmXkbGeometry;

#define SCM_CLASS_XKB_GEOMETRY       (&ScmXkbGeometry_class)
#define SCM_XKB_GEOMETRY(obj)        (((ScmXkbGeometry*) obj)->geometry)
#define SCM_XKB_GEOMETRY_P(obj)      Scm_TypeP(obj, SCM_CLASS_XKB_GEOMETRY)

extern ScmObj new_xkb_geometry(XkbGeometryPtr geometry);
ScmObj xkb_get_geometry (ScmXkbDesc* s_desc);



SCM_CLASS_DECL(ScmXkbGeometry_Section_class);

typedef struct ScmXkbGeometry_Section {
   SCM_HEADER;
   XkbSectionPtr section;
   ScmXkbGeometry* geometry;
} ScmXkbGeometry_Section;

#define SCM_CLASS_XKB_GEOMETRY_SECTION       (&ScmXkbGeometry_Section_class)
#define SCM_XKB_GEOMETRY_SECTION(obj)        (((ScmXkbGeometry_Section*) obj)->section)
#define SCM_XKB_GEOMETRY_SECTION_P(obj)      Scm_TypeP(obj, SCM_CLASS_XKB_GEOMETRY_SECTION)

extern ScmObj new_xkb_geometry_section(XkbSectionPtr section);





SCM_CLASS_DECL(ScmXkbGeometry_Row_class);

typedef struct ScmXkbGeometry_Row {
   SCM_HEADER;
   XkbRowPtr row;
   ScmXkbGeometry_Section *section;
} ScmXkbGeometry_Row;

#define SCM_CLASS_XKB_GEOMETRY_ROW       (&ScmXkbGeometry_Row_class)
#define SCM_XKB_GEOMETRY_ROW(obj)        (((ScmXkbGeometry_Row*) obj)->row)
#define SCM_XKB_GEOMETRY_ROW_P(obj)      Scm_TypeP(obj, SCM_CLASS_XKB_GEOMETRY_ROW)

extern ScmObj new_xkb_geometry_row(XkbRowPtr row);



SCM_CLASS_DECL(ScmXkbGeometry_Key_class);

typedef struct ScmXkbGeometry_Key {
   SCM_HEADER;
   XkbKeyPtr key;
   ScmXkbGeometry_Row* row;
} ScmXkbGeometry_Key;

#define SCM_CLASS_XKB_GEOMETRY_KEY       (&ScmXkbGeometry_Key_class)
#define SCM_XKB_GEOMETRY_KEY(obj)        (((ScmXkbGeometry_Key*) obj)->key)
#define SCM_XKB_GEOMETRY_KEY_P(obj)      Scm_TypeP(obj, SCM_CLASS_XKB_GEOMETRY_KEY)

extern ScmObj new_xkb_geometry_key(XkbKeyPtr key);


/**** Components  */
typedef struct ScmXkbComponentListRecRec {
    SCM_HEADER;
    XkbComponentListRec *data;
} ScmXkbComponentListRec;

SCM_CLASS_DECL(Scm_XkbComponentListRecClass);
#define SCM_CLASS_XKB_COMPONENT_LIST     (&Scm_XkbComponentListRecClass)
#define SCM_XKB_COMPONENT_LIST(obj)      (((ScmXkbComponentListRec*) obj)->data)
#define SCM_XKB_COMPONENT_LIST_P(obj)    (Scm_TypeP(obj, SCM_CLASS_XKB_COMPONENT_LIST))
extern ScmObj Scm_MakeXkbComponentListRec(XkbComponentListRec *data);





#endif /*   end */
