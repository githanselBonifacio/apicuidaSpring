package co.com.sura.postgres;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

public class Converter {

    public static <T> T deserializarJson (String json, Class<T> clazz){
        var mapper = new ObjectMapper().registerModule(new JavaTimeModule());;
        try {
            return mapper.readValue(json,clazz);
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


}
