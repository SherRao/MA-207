image_data = imread("deez", "jpg");
R = double(image_data(:,:,1));
G = double(image_data(:,:,2));
B = double(image_data(:,:,3));

[UR, SR, VR] = svd(R);
[UG, SG, VG] = svd(G);
[UB, SB, VB] = svd(B);

image1 = compress_image(UR, SR, VR, UG, SG, VG, UB, SB, VB, 1);
image2 = compress_image(UR, SR, VR, UG, SG, VG, UB, SB, VB, 2);
image3 = compress_image(UR, SR, VR, UG, SG, VG, UB, SB, VB, 3);
image4 = compress_image(UR, SR, VR, UG, SG, VG, UB, SB, VB, 4);
image5 = compress_image(UR, SR, VR, UG, SG, VG, UB, SB, VB, 5);
image15 = compress_image(UR, SR, VR, UG, SG, VG, UB, SB, VB, 15);
image25 = compress_image(UR, SR, VR, UG, SG, VG, UB, SB, VB, 25);
image35 = compress_image(UR, SR, VR, UG, SG, VG, UB, SB, VB, 35);
image45 = compress_image(UR, SR, VR, UG, SG, VG, UB, SB, VB, 45);
image75 = compress_image(UR, SR, VR, UG, SG, VG, UB, SB, VB, 75);  

function image = compress_image(UR, SR, VR, UG, SG, VG, UB, SB, VB, k)
    R_app = UR(:,1:k) * SR(1:k,1:k) * transpose(VR(:,1:k));
    G_app = UG(:,1:k) * SG(1:k,1:k) * transpose(VG(:,1:k));
    B_app = UB(:,1:k) * SB(1:k,1:k) * transpose(VB(:,1:k));
    
    image(:,:,1) = uint8(R_app);
    image(:,:,2) = uint8(G_app);
    image(:,:,3) = uint8(B_app);
end