package co.com.sura.entity.remision;

import co.com.sura.entity.maestro.HorarioTurno;
import co.com.sura.entity.personal.SecuenciaTurno;
import co.com.sura.entity.remision.datosremision.ItemDiaTurno;
import co.com.sura.entity.remision.datosremision.ItemSecuenciaTurno;

import java.util.Collections;
import java.util.List;

public interface RemisionFactory {
     static ItemDiaTurno crearItemDiaTurno(
            Integer numeroDia, String nombreDia, List<HorarioTurno> horariosTurno){
        return  ItemDiaTurno.builder()
                .numeroDia(numeroDia)
                .nombreDia(nombreDia)
                .horariosTurno(horariosTurno)
                .build();
    }

     static ItemDiaTurno crearItemDiaTurno(ItemSecuenciaTurno itemSecuenciaTurno){
        return ItemDiaTurno.builder()
                .numeroDia(itemSecuenciaTurno.getNumeroDia())
                .nombreDia(itemSecuenciaTurno.getNombreDia())
                .horariosTurno(itemSecuenciaTurno.getHorariosTurno())
                .build();
    }

     static SecuenciaTurno crearSecuenciaTurno(ItemSecuenciaTurno itemSecuenciaTurno, ItemDiaTurno itemsDiaTurno){
        return SecuenciaTurno.builder()
                .nombre(itemSecuenciaTurno.getNombreSecuencia())
                .descripcion(itemSecuenciaTurno.getDescripcion())
                .itemsDiaTurno(Collections.singletonList(itemsDiaTurno))
                .build();
    }

}
