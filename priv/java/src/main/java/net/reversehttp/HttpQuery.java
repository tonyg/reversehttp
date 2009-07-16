package net.reversehttp;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class HttpQuery {
    public interface KeyValueHandler {
        void handle(String key, String value);
    }

    public static void foreachKeyValue(String queryStr, KeyValueHandler handler) {
        if (queryStr != null) {
            for (String item : queryStr.split("&")) {
                String[] pieces = item.split("=");
                try {
                    String key = URLDecoder.decode(pieces[0], "UTF-8");
                    String value = pieces.length > 1 ? URLDecoder.decode(pieces[1], "UTF-8") : "";
                    handler.handle(key, value);
                } catch (UnsupportedEncodingException uee) {
                    throw new RuntimeException(uee);
                }
            }
        }
    }

    public static Map<String, String> parse(String queryStr) {
        final Map<String, String> result = new HashMap<String, String>();
        foreachKeyValue(queryStr, new KeyValueHandler() {
            public void handle(String key, String value) {
                result.put(key, value);
            }
        });
        return result;
    }

    public static Map<String, List<String>> parseMultiple(String queryStr) {
        final Map<String, List<String>> result = new HashMap<String, List<String>>();
        foreachKeyValue(queryStr, new KeyValueHandler() {
            public void handle(String key, String value) {
                if (!result.containsKey(key)) {
                    result.put(key, new ArrayList<String>());
                }
                result.get(key).add(value);
            }
        });
        return result;
    }
    
    private static boolean appendEntry(StringBuilder sb, boolean needAmpersand,
            String key, String value)
    {
        try {
            if (needAmpersand) { sb.append('&'); }
            sb.append(URLEncoder.encode(key, "UTF-8"));
            sb.append('=');
            sb.append(URLEncoder.encode(value, "UTF-8"));
            return true;
        } catch (UnsupportedEncodingException uee) {
            throw new RuntimeException(uee);
        }
    }

    public static String unparse(Map<String, String> parsedQuery) {
        StringBuilder sb = new StringBuilder();
        boolean needAmpersand = false;
        for (Map.Entry<String, String> e : parsedQuery.entrySet()) {
            needAmpersand = appendEntry(sb, needAmpersand, e.getKey(), e.getValue());
        }
        return sb.toString();
    }

    public static String unparseMultiple(Map<String, List<String>> parsedQuery) {
        StringBuilder sb = new StringBuilder();
        boolean needAmpersand = false;
        for (Map.Entry<String, List<String>> e : parsedQuery.entrySet()) {
            for (String value : e.getValue()) {
                needAmpersand = appendEntry(sb, needAmpersand, e.getKey(), value);
            }
        }
        return sb.toString();
    }
}