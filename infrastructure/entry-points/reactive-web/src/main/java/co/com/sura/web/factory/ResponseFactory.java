package co.com.sura.web.factory;

import co.com.sura.genericos.Response;

public class ResponseFactory {

    public static <T> Response<T> createStatus(
            T result, Integer status, String detail, String mensaje, String dsMensaje){

        return Response.<T>builder()
                .status(status)
                .error(null)
                .message(mensaje)
                .tecnicalMessage(dsMensaje)
                .detail(detail)
                .flag(true)
                .result(result)
                .build();
    }
}
