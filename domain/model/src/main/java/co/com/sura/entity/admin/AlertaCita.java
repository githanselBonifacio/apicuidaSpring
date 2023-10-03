package co.com.sura.entity.admin;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class AlertaCita {
    private String textoAlerta;
    private boolean cambiarDuracion;
    private Integer duracion;
}
