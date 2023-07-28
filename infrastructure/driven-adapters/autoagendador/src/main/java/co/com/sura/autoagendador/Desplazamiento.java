package co.com.sura.autoagendador;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Desplazamiento {
    private String pos1;
    private String pos2;
    private Integer tiempo;

    public static Desplazamiento crearDesplazamiento(CitaGenetic citaPartida,CitaGenetic citaDestino,Integer duracion){
        return  new Desplazamiento()
                .toBuilder()
                .pos1(citaPartida.getIdCita())
                .pos2(citaDestino.getIdCita())
                .tiempo(duracion)
                .build();
    }
}
