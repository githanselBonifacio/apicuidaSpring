package co.com.sura.postgres;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.springframework.stereotype.Component;

@Component
public class Converter {

    public static <T> T deserializarJson (String json, Class<T> clazz){
        var mapper = new ObjectMapper().registerModule(new JavaTimeModule());
        try {
            return mapper.readValue(toCamelCase(json),clazz);
        } catch (JsonProcessingException e) {
            return null;
        }
    }

    public static String  convertirObjetoAJson(Object object){
        var mapper = new ObjectMapper().registerModule(new JavaTimeModule());

        try {
            return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(object);
        } catch (JsonProcessingException e) {
            return null;
        }
    }
    private static String toCamelCase(String str) {
        var builder = new StringBuilder();
        var capitalizeNext = false;

        for (char c : str.toCharArray()) {
            if (c == '_') {
                capitalizeNext = true;
            } else {
                if (capitalizeNext) {
                    builder.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else {
                    builder.append(c);
                }
            }
        }

        return builder.toString();
    }
   public static <T> T converToEntity(Object entity,Class<T> clazz ){
       return deserializarJson(convertirObjetoAJson(entity), clazz);
   }
}
