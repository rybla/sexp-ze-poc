# sexp-ze-poc

the current version is a good start, but there are some major issues:
- moving cursor is cumbersome since you have to inchworm the sides
- should behave like text editing controls when not doing zipper selections 
- space bar is unintuitive


fixes:
- arrow keys should move empty span cursor around
- there should be no seperate "zipper selection mode" where you are placing the other end of a zipper selection, which could be in an invalid position
  - instead, you should be moving around the other end of the zipper, while each intermediate state is a valid zipper position
- hold Shift while moving to keep marker in place and move secondary cursor