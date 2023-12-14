package co.com.sura.maestros.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Regional {
    private String id;
    private String nombre;
    private Double latitud;
    private Double longitud;
    private String direccion;
}
