package co.com.sura.entity.remision.datosremision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Diagnostico {
    private String codigo;
    private String nombre;
}
