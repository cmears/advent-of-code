package advent2016;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.HashMap;

public class Day14 {
    public static String md5(String input) {
        try {
            byte[] digest = MessageDigest.getInstance("MD5").digest(input.getBytes());
            return String.format("%032x", new BigInteger(1, digest));
        } catch (Exception e) { return null; }
    }

    public static String md5_stretched(String input) {
        String hash = md5(input);
        for (int i = 0 ; i < 2016 ; i++)
            hash = md5(hash);
        return hash;
    }

    public static String cached_stretched(int index) {
        if (cache.containsKey(index))
            return cache.get(index);
        String hash = md5_stretched(salt + index);
        cache.put(index, hash);
        return hash;
    }

    static HashMap<Integer, String> cache = new HashMap<>();
    static String salt = "jlmsuwbz";

    public static void main(String[] args) {
        int index = 0;
        int keys_found = 0;
        forever:
        for (;;) {
            String hash = cached_stretched(index);
            find_triplet:
            for (int i = 0 ; i < hash.length()-2 ; i++) {
                if (hash.charAt(i) == hash.charAt(i+1) &&
                    hash.charAt(i) == hash.charAt(i+2)) {
                    char target = hash.charAt(i);
                    find_quintet:
                    for (int j = index+1 ; j < index + 1001 ; j++) {
                        String hashj = cached_stretched(j);
                        for (int k = 0 ; k < hashj.length()-4 ; k++) {
                            if (hashj.charAt(k) == target &&
                                    hashj.charAt(k + 1) == target &&
                                    hashj.charAt(k + 2) == target &&
                                    hashj.charAt(k + 3) == target &&
                                    hashj.charAt(k + 4) == target) {
                                keys_found++;
                                System.out.println(index);
                                if (keys_found == 64)
                                    break forever;
                                break find_quintet;
                            }
                        }
                    }
                    break find_triplet;
                }
            }
            index++;
        }
    }
}
